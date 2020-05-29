package dddblueprint

import alleycats.Zero
import cats.{ ~>, FlatMap }
import cats.data.StateT

trait Conversions {

  implicit def runState[IO[_]: FlatMap, State]: Zero[State] => (StateT[IO, State, *] ~> IO) =
    zero =>
      new (StateT[IO, State, *] ~> IO) {

        def apply[A](stateIO: StateT[IO, State, A]): IO[A] = stateIO.runA(zero.zero)
      }

  implicit class ListCollOps[A](val traversable: TraversableOnce[A]) {

    def toListSet: ListSet[A] = traversable.to(ListSet)
  }
}
