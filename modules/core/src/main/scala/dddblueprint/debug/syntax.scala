package dddblueprint
package debug

import cats.syntax.show._
import cats.Show

object syntax { // scalastyle:ignore object.name

  trait ShowLifted { def value(): String }

  implicit def liftWithShow[A: Show](value: A): ShowLifted = () => value.show

  trait LogWithF { def apply[F[_]: Logging]: F[Unit] }

  implicit class LoggerContext(val sc: StringContext) extends AnyVal {

    def debug(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].debug(sc.raw(args.map(_.value()): _*))
    }
    def info(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].info(sc.raw(args.map(_.value()): _*))
    }
    def warn(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].warn(sc.raw(args.map(_.value()): _*))
    }
    def error(args: ShowLifted*): LogWithF = new LogWithF {
      def apply[F[_]: Logging]: F[Unit] = Logging[F].error(sc.raw(args.map(_.value()): _*))
    }
  }
}