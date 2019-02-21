package dddblueprint
package monix

import cats.implicits._
import _root_.monix.eval.Task
import cats.data.{ NonEmptyList, StateT }
import cats.{ Applicative, ApplicativeError, Functor }

object WithTaskState {

  implicit val taskSchemaErrorHandle: SchemaErrorHandle[Task] =
    new SchemaErrorHandle[Task] {
      val applicative: Applicative[Task] = Applicative[Task]
      val functor:     Functor[Task]     = Functor[Task]

      def attempt[A](fa: Task[A]): Task[Either[NonEmptyList[SchemaError], A]] =
        ApplicativeError[Task, Throwable].attempt(fa).flatMap {
          case Left(SchemaError.Wrapper(errors)) => errors.asLeft[A].pure[Task]
          case Left(throwable)                   => ApplicativeError[Task, Throwable].raiseError(throwable)
          case Right(value)                      => value.asRight[NonEmptyList[SchemaError]].pure[Task]
        }
      def handle[A](fa: Task[A])(f: NonEmptyList[SchemaError] => A): Task[A] =
        ApplicativeError[Task, Throwable].handleErrorWith(fa) {
          case SchemaError.Wrapper(errors) => f(errors).pure[Task]
          case _: Throwable => fa
        }
      def handleWith[A](fa: Task[A])(f: NonEmptyList[SchemaError] => Task[A]): Task[A] =
        ApplicativeError[Task, Throwable].handleErrorWith(fa) {
          case SchemaError.Wrapper(errors) => f(errors)
          case _: Throwable => fa
        }
      def raise[A](e: NonEmptyList[SchemaError]): Task[A] =
        ApplicativeError[Task, Throwable].raiseError(SchemaError.Wrapper(e))
    }

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
