package dddblueprint
package logback

import cats.effect.Sync
import com.typesafe.scalalogging.Logger
import io.scalaland.pulp.{ Cached, Provider }

@Cached final class LogbackLogging[F[_]: Sync] extends debug.Logging[F] {

  private val logger = Logger("dddblueprint")

  def trace(msg: String): F[Unit] = Sync[F].delay(logger.trace(msg))
  def debug(msg: String): F[Unit] = Sync[F].delay(logger.debug(msg))
  def info(msg:  String): F[Unit] = Sync[F].delay(logger.info(msg))
  def warn(msg:  String): F[Unit] = Sync[F].delay(logger.warn(msg))
  def error(msg: String): F[Unit] = Sync[F].delay(logger.error(msg))
  def error(msg: String, ex: Throwable): F[Unit] = Sync[F].delay(logger.error(msg, ex))
}

object LogbackLogging {

  trait Module {

    def logbackLogging[F[_]](implicit ev: Provider[LogbackLogging[F]]): Provider[debug.Logging[F]] =
      Provider.upcast[LogbackLogging[F], debug.Logging[F]]
  }
}
