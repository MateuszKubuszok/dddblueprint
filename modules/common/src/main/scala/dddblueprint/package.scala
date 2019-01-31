import cats.{ Applicative, Eq, Eval, Show, Traverse }

import cats.implicits._

import scala.collection.immutable.{ ListMap, ListSet }

package object dddblueprint {

  // missing type classes for our own convenience

  private[dddblueprint] implicit def eqListMap[K, V](implicit eqMap: Eq[Map[K, V]]): Eq[ListMap[K, V]] =
    (a, b) => eqMap.eqv(a, b)
  private[dddblueprint] implicit def eqListSet[A](implicit eqSet: Eq[Set[A]]): Eq[ListSet[A]] =
    (a, b) => eqSet.eqv(a, b)

  private[dddblueprint] implicit def foldableListMap[K]: Traverse[ListMap[K, ?]] = new Traverse[ListMap[K, ?]] {
    def traverse[G[_], V, B](fa: ListMap[K, V])(f: V => G[B])(implicit ev: Applicative[G]): G[ListMap[K, B]] =
      fa.foldLeft(ListMap.empty[K, B].pure[G]) {
        case (out, (k, v)) =>
          out.map2(f(v)) { (listB, b) =>
            listB + (k -> b)
          }
      }

    def foldLeft[V, W](fa: ListMap[K, V], b: W)(f: (W, V) => W): W =
      fa.foldLeft(b) { case (w, (_, v)) => f(w, v) }

    @SuppressWarnings(Array("org.wartremover.warts.Recursion", "org.wartremover.warts.TraversableOps"))
    def foldRight[V, W](fa: ListMap[K, V], lb: Eval[W])(f: (V, Eval[W]) => Eval[W]): Eval[W] = {
      def loop(as: ListMap[K, V]): Eval[W] =
        if (as.isEmpty) lb
        else f(as.head._2, Eval.defer(loop(as.tail)))
      Eval.defer(loop(fa))
    }
  }
  private[dddblueprint] implicit val foldableListSet: Traverse[ListSet] = new Traverse[ListSet] {
    def traverse[G[_], A, B](fa: ListSet[A])(f: A => G[B])(implicit ev: Applicative[G]): G[ListSet[B]] =
      fa.foldLeft(ListSet.empty[B].pure[G]) { (out, a) =>
        out.map2(f(a)) { (listB, b) =>
          listB + b
        }
      }

    def foldLeft[A, B](fa: ListSet[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    @SuppressWarnings(Array("org.wartremover.warts.Recursion", "org.wartremover.warts.TraversableOps"))
    def foldRight[A, B](fa: ListSet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: ListSet[A]): Eval[B] =
        if (as.isEmpty) lb
        else f(as.head, Eval.defer(loop(as.tail)))
      Eval.defer(loop(fa))
    }
  }

  private[dddblueprint] implicit def showListMap[K, V](implicit showMap: Show[Map[K, V]]): Show[ListMap[K, V]] =
    listMap => showMap.show(listMap)
  private[dddblueprint] implicit def showListSet[A](implicit showSet: Show[Set[A]]): Show[ListSet[A]] =
    listSet => showSet.show(listSet)
}
