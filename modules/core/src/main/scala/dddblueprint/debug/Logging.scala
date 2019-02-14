package dddblueprint
package debug
import cats.Eval

trait Logging[F[_]] {

  def debug(msg: String): F[Unit]
  def info(msg:  String): F[Unit]
  def warn(msg:  String): F[Unit]
  def error(msg: String): F[Unit]
  def error(msg: String, ex: Throwable): F[Unit]
}

object Logging {

  implicit val evalLogging = new Logging[cats.Eval] {
    def debug(msg: String): Eval[Unit] = cats.Eval.later(println(msg))
    def info(msg:  String): Eval[Unit] = cats.Eval.later(println(msg))
    def warn(msg:  String): Eval[Unit] = cats.Eval.later(println(msg))
    def error(msg: String): Eval[Unit] = cats.Eval.later(println(msg))
    def error(msg: String, ex: Throwable): Eval[Unit] = cats.Eval.later(println(msg))
  }

  @inline def apply[F[_]](implicit logging: Logging[F]): Logging[F] = logging
}
