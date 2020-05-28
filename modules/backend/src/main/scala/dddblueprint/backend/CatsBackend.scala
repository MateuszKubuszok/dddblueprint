package dddblueprint
package backend

import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import cats.Traverse

import scala.meta._
import output._

final class CatsBackend[IO[_]: Sync: SchemaErrorRaise](pkg: String) extends Backend[IO] {

  implicit class StringOps(string: String) {

    def asPackageName: String = string.toLowerCase.replaceAll("[- ]", "_")
    def asClassName:   String = string.replaceAll("[- ]", "_")
  }

  def apply(blueprint: Blueprint): IO[ListMap[Path, String]] = ???

  def getFullName(snapshot: Snapshot)(ref: DefinitionRef): IO[Type] =
    snapshot
      .findDomainNameAndName(ref)
      .map {
        case (domainName, name) =>
          val version = snapshot.namespaces.versions(ref)
          s"$pkg.${domainName.asPackageName}.${name.asClassName}.$version".parse[Type].get.pure[IO]
      }
      .getOrElse(SchemaError.invalidRef(ref.id))

  val getPrimitiveType: Data.Primitive => IO[Type] = Map(
    Data.UUID -> "java.util.UUID".parse[Type].get.pure[IO],
    Data.Boolean -> "scala.Boolean".parse[Type].get.pure[IO],
    Data.Int -> "scala.Int".parse[Type].get.pure[IO],
    Data.Long -> "scala.Long".parse[Type].get.pure[IO],
    Data.Float -> "scala.Float".parse[Type].get.pure[IO],
    Data.Double -> "scala.Double".parse[Type].get.pure[IO],
    Data.String -> "scala.Predef.String".parse[Type].get.pure[IO]
  )

  def getCollectionType(snapshot: Snapshot): Data.Collection => IO[Type] = {
    val argument: Argument => IO[Type] = getArgumentType(snapshot)
    ({
      case Data.Collection.Option(of) => argument(of).map(tpe => s"scala.Option[$tpe]".parse[Type].get)
      case Data.Collection.Array(of)  => argument(of).map(tpe => s"scala.List[$tpe]".parse[Type].get)
      case Data.Collection.Set(of) =>
        argument(of).map(tpe => s"scala.Predef.Set[$tpe]".parse[Type].get)
      case Data.Collection.Map(k, v) =>
        (argument(k) -> argument(v)).mapN((ktpe, vtpe) => s"scala.Predef.Map[$ktpe, $vtpe]".parse[Type].get)
    }: Data.Collection => IO[Type])
  }

  def getTupleType(snapshot: Snapshot): Data.Tuple => IO[Type] = {
    val argument: Argument => IO[Type] = getArgumentType(snapshot)
    ({
      case Data.Tuple(args) =>
        Traverse[List].sequence(args.map(argument)).map(types => s"(${types.mkString(",")})".parse[Type].get)
    }: Data.Tuple => IO[Type])
  }

  def getArgumentType(snapshot: Snapshot): Argument => IO[Type] = {
    case r: DefinitionRef   => getFullName(snapshot)(r)
    case p: Data.Primitive  => getPrimitiveType(p)
    case c: Data.Collection => getCollectionType(snapshot)(c)
    case t: Data.Tuple      => getTupleType(snapshot)(t)
  }

  // TODO
  def getVersionedDefinitionBody(snapshot: Snapshot): Data.Definition => IO[Source] = { _ =>
    snapshot.hashCode()
    "{}".parse[Source].get.pure[IO]
  }
  /*
  def getVersionedDefinitionBody(snapshot: Snapshot): Data.Definition => IO[Source] = {
    case Data.Definition.Record.Entity(ref, fields)  => "".parse[Source].get.pure[IO]
    case Data.Definition.Record.Value(ref, fields)   => "".parse[Source].get.pure[IO]
    case Data.Definition.Record.Event(ref, fields)   => "".parse[Source].get.pure[IO]
    case Data.Definition.Service(ref, input, output) => "".parse[Source].get.pure[IO]
    case Data.Definition.Publisher(ref, events)      => "".parse[Source].get.pure[IO]
    case Data.Definition.Subscriber(ref, events)     => "".parse[Source].get.pure[IO]
  }
   */

  // TODO: rename handling?!?! (actually it can be done with getFullName)

  // TODO: create automatic migrations that do not require manual intervention
}

object CatsBackend {}
