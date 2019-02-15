package dddblueprint
package debug

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
}
