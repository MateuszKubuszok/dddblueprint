package dddblueprint
package monix

import cats.implicits._
import cats.data.{ NonEmptyList, StateT }
import cats.{ Applicative, ApplicativeError, Functor }
import _root_.monix.eval.Coeval

object WithCoeval {

  type CoevalState[A] = StateT[Coeval, output.Snapshot, A]

  implicit val taskStateSchemaErrorHandle: SchemaErrorHandle[CoevalState] =
    new SchemaErrorHandle[CoevalState] {
      val applicative: Applicative[CoevalState] = Applicative[CoevalState]
      val functor:     Functor[CoevalState]     = Functor[CoevalState]

      def attempt[A](fa: CoevalState[A]): CoevalState[Either[NonEmptyList[SchemaError], A]] =
        ApplicativeError[CoevalState, Throwable].attempt(fa).flatMap {
          case Left(SchemaError.Wrapper(errors)) => errors.asLeft[A].pure[CoevalState]
          case Left(throwable)                   => ApplicativeError[CoevalState, Throwable].raiseError(throwable)
          case Right(value)                      => value.asRight[NonEmptyList[SchemaError]].pure[CoevalState]
        }
      def handle[A](fa: CoevalState[A])(f: NonEmptyList[SchemaError] => A): CoevalState[A] =
        ApplicativeError[CoevalState, Throwable].handleErrorWith(fa) {
          case SchemaError.Wrapper(errors) => f(errors).pure[CoevalState]
          case _: Throwable => fa
        }
      def handleWith[A](fa: CoevalState[A])(f: NonEmptyList[SchemaError] => CoevalState[A]): CoevalState[A] =
        ApplicativeError[CoevalState, Throwable].handleErrorWith(fa) {
          case SchemaError.Wrapper(errors) => f(errors)
          case _: Throwable => fa
        }
      def raise[A](e: NonEmptyList[SchemaError]): CoevalState[A] =
        ApplicativeError[CoevalState, Throwable].raiseError(SchemaError.Wrapper(e))
    }
}
