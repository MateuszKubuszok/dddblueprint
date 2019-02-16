package dddblueprint
package parser

import java.io.File
import java.net.URLDecoder
import java.nio.file.Files
import java.util.jar.JarFile

import cats.implicits._
import cats.Traverse

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

// scalastyle:off magic.number
trait ParserSpec extends FRunningSpec {

  def load(path: String): ListMap[String, String] = {
    val cl = getClass.getClassLoader

    lazy val fileUrl =
      Option(cl.getResource(path)).filter(_.getProtocol == "file").map(url => new File(url.toURI).list.toList)

    lazy val jarUrl =
      Option(getClass.getName.replace('.', '/') + ".class").map(cl.getResource).filter(_.getProtocol == "jar").map {
        url =>
          val jarPath = url.getPath.substring(5, url.getPath.indexOf('!')) // remove JAR part
          val jar     = new JarFile(URLDecoder.decode(jarPath, "UTF-8"))
          jar.entries.asScala.toList.filter(_.getName.startsWith(path)).map { entry =>
            val entryName   = entry.getName.substring(path.length)
            val subdirIndex = entryName.indexOf('/')
            if (subdirIndex >= 0) entryName.substring(0, subdirIndex) else entryName
          }
      }

    val result = fileUrl.orElse(jarUrl)
    result must beSome[List[String]]
    implicit val ordering: Ordering[String] = new NaturalLanguageOrdering
    ListMap(
      result.get.sorted.map { name =>
        name -> new String(Files.readAllBytes(new File(cl.getResource(s"$path/$name").getFile).toPath))
      }: _*
    )
  }

  abstract class TestParsing(migrations: List[F[input.Migration]]) {
    protected val result =
      Traverse[List].sequence[F, input.Migration](migrations).map(input.History(_))
    lazy val history = result.runA(output.Snapshot()).apply()
  }
}
