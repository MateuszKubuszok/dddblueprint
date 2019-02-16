package dddblueprint
package parser

import java.io.File
import java.net.URLDecoder
import java.nio.file.Files
import java.util.jar.JarFile

import cats.effect.Sync
import cats.implicits._
import cats.Traverse
import io.scalaland.pulp.Cached

import scala.collection.JavaConverters._

@Cached class DirectoryParser[F[_]: Sync: SchemaErrorRaise: Parser] {

  def apply(path: String): F[input.History] =
    load(path).flatMap {
      case Some(inputs) =>
        Traverse[List].sequence[F, input.Migration](inputs.map(Parser[F].apply(_))).map(input.History(_))
      case None =>
        SchemaError.parsingError[F, input.History](s"Not found migrations in $path")
    }

  def load(path: String): F[Option[List[String]]] = Sync[F].delay {
    val cl = getClass.getClassLoader

    lazy val external =
      Option(new File(path)).filter(_.exists).filter(_.isDirectory).map(_.listFiles.toSet)

    lazy val fileUrl =
      Option(cl.getResource(path)).filter(_.getProtocol === "file").map(url => new File(url.toURI).listFiles.toSet)

    lazy val jarUrl =
      Option(getClass.getName.replace('.', '/') + ".class").map(cl.getResource).filter(_.getProtocol === "jar").map {
        url =>
          val jarPath = url.getPath.substring("jar:/".length, url.getPath.indexOf('!')) // remove JAR part
          val jar     = new JarFile(URLDecoder.decode(jarPath, "UTF-8"))
          jar.entries.asScala.toSet
            .filter(_.getName.startsWith(path))
            .map { entry =>
              val entryName   = entry.getName.substring(path.length)
              val subdirIndex = entryName.indexOf('/')
              if (subdirIndex >= 0) entryName.substring(0, subdirIndex) else entryName
            }
            .map(name => new File(cl.getResource(s"$path/$name").getFile))
      }

    implicit val ordering: Ordering[String] = new NaturalLanguageOrdering
    external orElse fileUrl orElse jarUrl map { result =>
      result.toList.sortBy(_.getName).map(file => new String(Files.readAllBytes(file.toPath)))
    }
  }
}

object DirectoryParser {

  def apply[F[_]](implicit directoryParser: DirectoryParser[F]): DirectoryParser[F] = directoryParser
}
