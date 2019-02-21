package dddblueprint
package debug

import cats.Applicative
import cats.implicits._

trait Logging[F[_]] {

  def trace(msg: String): F[Unit]
  def debug(msg: String): F[Unit]
  def info(msg:  String): F[Unit]
  def warn(msg:  String): F[Unit]
  def error(msg: String): F[Unit]
  def error(msg: String, ex: Throwable): F[Unit]
}

object Logging {

  @inline def apply[F[_]](implicit logging: Logging[F]): Logging[F] = logging

  def noop[F[_]: Applicative]: Logging[F] = new Logging[F] {
    def trace(msg: String): F[Unit] = ().pure[F]
    def debug(msg: String): F[Unit] = ().pure[F]
    def info(msg:  String): F[Unit] = ().pure[F]
    def warn(msg:  String): F[Unit] = ().pure[F]
    def error(msg: String): F[Unit] = ().pure[F]
    def error(msg: String, ex: Throwable): F[Unit] = ().pure[F]
  }
}
