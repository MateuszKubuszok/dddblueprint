package dddblueprint
package monix

import cats.implicits._
import _root_.monix.eval.Task
import cats.data.{ NonEmptyList, StateT }
import cats.{ Applicative, ApplicativeError, Functor }

object WithTaskState {

  type TaskState[A] = StateT[Task, output.Snapshot, A]

  implicit val taskStateSchemaErrorHandle: SchemaErrorHandle[TaskState] =
    new SchemaErrorHandle[TaskState] {
      val applicative: Applicative[TaskState] = Applicative[TaskState]
      val functor:     Functor[TaskState]     = Functor[TaskState]

      def attempt[A](fa: TaskState[A]): TaskState[Either[NonEmptyList[SchemaError], A]] =
        ApplicativeError[TaskState, Throwable].attempt(fa).flatMap {
          case Left(SchemaError.Wrapper(errors)) => errors.asLeft[A].pure[TaskState]
          case Left(throwable)                   => ApplicativeError[TaskState, Throwable].raiseError(throwable)
          case Right(value)                      => value.asRight[NonEmptyList[SchemaError]].pure[TaskState]
        }
      def handle[A](fa: TaskState[A])(f: NonEmptyList[SchemaError] => A): TaskState[A] =
        ApplicativeError[TaskState, Throwable].handleErrorWith(fa) {
          case SchemaError.Wrapper(errors) => f(errors).pure[TaskState]
          case _: Throwable => fa
        }
      def handleWith[A](fa: TaskState[A])(f: NonEmptyList[SchemaError] => TaskState[A]): TaskState[A] =
        ApplicativeError[TaskState, Throwable].handleErrorWith(fa) {
          case SchemaError.Wrapper(errors) => f(errors)
          case _: Throwable => fa
        }
      def raise[A](e: NonEmptyList[SchemaError]): TaskState[A] =
        ApplicativeError[TaskState, Throwable].raiseError(SchemaError.Wrapper(e))
    }
}
