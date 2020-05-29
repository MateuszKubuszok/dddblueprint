package dddblueprint
package backend

import java.nio.file.{ Path, Paths }

import cats.effect.Sync
import cats.implicits._
import cats.Traverse
import io.scalaland.pulp._

import scala.meta._
import output._

import scala.reflect._

final class CatsBackend[IO[_]: Sync: SchemaErrorRaise](pkg: String) extends Backend[IO] {

  implicit class StringOps(string: String) {

    def asPackageName: String = string.toLowerCase.replaceAll("[- ]", "_")
    def asClassName:   String = string.replaceAll("[- ]", "_")

    // TODO: use better error handling
    def parseIO[U: scala.meta.parsers.Parse: ClassTag]: IO[U] = Sync[IO].delay(string.parse[U].get).recoverWith {
      case error: Exception =>
        Sync[IO].raiseError((new Exception(show"Original code:\n$string\n${classTag[U].toString}", error)))
    }
  }

  def apply(blueprint: Blueprint): IO[ListMap[Path, String]] =
    blueprint.versions
      .flatTraverse { snapshot: Snapshot =>
        /*
         TODO:
         1. foreach snapshot
         2. - generate ADT for each type
         3. - generate trait for each service
         4. - generate migration
         */
        val versioned = getVersionedDefinitionBody(snapshot)
        snapshot.definitions.toList.traverse {
          case (ref, definition) =>
            versioned(definition).map(Paths.get(ref.id.toString) -> _.toString)
        }
      }
      .map(l => ListMap(l.toSeq: _*))

  def getFullName(snapshot: Snapshot)(ref: DefinitionRef): IO[Type] =
    snapshot
      .findDomainNameAndName(ref)
      .map {
        case (domainName, name) =>
          show"$pkg.${domainName.asPackageName}.${name.asClassName}.v${snapshot.namespaces.versions(ref)}".parseIO[Type]
      }
      .getOrElse(SchemaError.invalidRef(ref.id))

  val getPrimitiveType: Data.Primitive => IO[Type] = Map(
    Data.UUID -> "java.util.UUID".parseIO[Type],
    Data.Boolean -> "scala.Boolean".parseIO[Type],
    Data.Int -> "scala.Int".parseIO[Type],
    Data.Long -> "scala.Long".parseIO[Type],
    Data.Float -> "scala.Float".parseIO[Type],
    Data.Double -> "scala.Double".parseIO[Type],
    Data.String -> "scala.Predef.String".parseIO[Type]
  )

  def getCollectionType(snapshot: Snapshot): Data.Collection => IO[Type] = {
    val argument: Argument => IO[Type] = getArgumentType(snapshot)
    ({
      case Data.Collection.Option(of) => argument(of).flatMap(tpe => show"scala.Option[${tpe.toString}]".parseIO[Type])
      case Data.Collection.Array(of)  => argument(of).flatMap(tpe => show"scala.List[${tpe.toString}]".parseIO[Type])
      case Data.Collection.Set(of) =>
        argument(of).flatMap(tpe => show"scala.Predef.Set[${tpe.toString}]".parseIO[Type])
      case Data.Collection.Map(k, v) =>
        (argument(k) -> argument(v))
          .mapN((ktpe, vtpe) => show"scala.Predef.Map[${ktpe.toString}, ${vtpe.toString}]".parseIO[Type])
          .flatten
    }: Data.Collection => IO[Type])
  }

  def getTupleType(snapshot: Snapshot): Data.Tuple => IO[Type] = {
    val argument: Argument => IO[Type] = getArgumentType(snapshot)
    ({
      case Data.Tuple(args) =>
        Traverse[List].sequence(args.map(argument)).flatMap(types => show"(${types.mkString(",")})".parseIO[Type])
    }: Data.Tuple => IO[Type])
  }

  def getArgumentType(snapshot: Snapshot): Argument => IO[Type] = {
    case r: DefinitionRef   => getFullName(snapshot)(r)
    case p: Data.Primitive  => getPrimitiveType(p)
    case c: Data.Collection => getCollectionType(snapshot)(c)
    case t: Data.Tuple      => getTupleType(snapshot)(t)
  }

  def getVersionedDefinitionBody(snapshot: Snapshot): Data.Definition => IO[Source] = {
    val toType: Argument => IO[Type] = getArgumentType(snapshot)
    def ver(ref:     DefinitionRef) = "v" + snapshot.namespaces.versions(ref).toString
    def args(fields: Data.Definition.FieldSet) =
      fields.toList
        .traverse { case (name, arg) => toType(arg).map(tpe => show"""$name: ${tpe.toString}""") }
        .map(_.mkString(", "))
    def out(output: Data.Definition.RefSet) =
      output.toList.traverse(out => toType(out).map(tpe => show"""${tpe.toString}""")).map(_.mkString(", "))
    // TODO: add also ver and type  to output type if needed!!!!
    ({
      case Data.Definition.Enum(ref, values, _) =>
        val v = ver(ref)
        show"""
            sealed trait $v extends Product with Serializable
            object $v {
              ${values.map("case object " + _ + show" extends $v").mkString("\n")}
            }
            """.parseIO[Source]
      case Data.Definition.Record.Entity(ref, fields) =>
        // TODO: add ID
        args(fields).flatMap(a => show"""final case class ${ver(ref)}($a)""".parseIO[Source])
      case Data.Definition.Record.Value(ref, fields) =>
        args(fields).flatMap(a => show"""final case class ${ver(ref)}($a)""".parseIO[Source])
      case Data.Definition.Record.Event(ref, fields) =>
        args(fields).flatMap(a => show"""final case class ${ver(ref)}($a)""".parseIO[Source])
      case Data.Definition.Service(ref, input, output) =>
        (args(input), out(output)).tupled.flatMap { case (a, o) => show"""def ${ver(ref)}($a): ($o)""".parseIO[Source] }
      case Data.Definition.Publisher(ref, events) =>
        out(events).flatMap(o => show"""type ${ver(ref)} = Publisher[($o)]""".parseIO[Source])
      case Data.Definition.Subscriber(ref, events) =>
        out(events).flatMap(o => show"""type ${ver(ref)} = Subscriber[($o)]""".parseIO[Source])
    }: Data.Definition => IO[Source])
  }
}
