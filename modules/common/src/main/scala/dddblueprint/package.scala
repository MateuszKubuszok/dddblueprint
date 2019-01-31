import cats.{ Eq, Show }

import scala.collection.immutable.{ ListMap, ListSet }

package object dddblueprint {

  private[dddblueprint] implicit def eqListMap[K, V](implicit eqMap: Eq[Map[K, V]]): Eq[ListMap[K, V]] =
    (a, b) => eqMap.eqv(a, b)
  private[dddblueprint] implicit def eqListSet[A](implicit eqSet: Eq[Set[A]]): Eq[ListSet[A]] =
    (a, b) => eqSet.eqv(a, b)

  private[dddblueprint] implicit def showListMap[K, V](implicit showMap: Show[Map[K, V]]): Show[ListMap[K, V]] =
    listMap => showMap.show(listMap)
  private[dddblueprint] implicit def showListSet[A](implicit showSet: Show[Set[A]]): Show[ListSet[A]] =
    listSet => showSet.show(listSet)
}
