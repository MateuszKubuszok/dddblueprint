package dddblueprint
package debug

import cats.syntax.show._
import cats.Show

import scala.annotation.implicitNotFound

object syntax { // scalastyle:ignore object.name

  @implicitNotFound("Couldn't find cats.Show to lift - provide one or, call .toString explicitly")
  trait ShowLifted { def value(): String }

  implicit def liftWithShow[A: Show](value: A): ShowLifted = () => value.show

  trait LogWithF { def apply[F[_]:                        Logging]: F[Unit] }
  trait LogErrorWithF extends LogWithF { def withEx[F[_]: Logging](ex: Throwable): F[Unit] }

  implicit final class LoggerContext(val sc: StringContext) extends AnyVal {

    def debug(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].debug(sc.raw(args.map(_.value()): _*))
    }
    def info(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].info(sc.raw(args.map(_.value()): _*))
    }
    def warn(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].warn(sc.raw(args.map(_.value()): _*))
    }
    def error(args: ShowLifted*): LogErrorWithF = new LogErrorWithF {
      def apply[F[_]:  Logging]: F[Unit] = Logging[F].error(sc.raw(args.map(_.value()): _*))
      def withEx[F[_]: Logging](ex: Throwable): F[Unit] = Logging[F].error(sc.raw(args.map(_.value()): _*), ex)
    }
  }
}
