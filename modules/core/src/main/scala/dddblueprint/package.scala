import cats.{ Applicative, Eq, Eval, Traverse }
import cats.derived.ShowPretty
import cats.implicits._
import monocle.function._

import scala.collection.immutable.{ ListMap, ListSet }

package object dddblueprint {

  // missing type classes for our own convenience

  implicit def atListMap[K, V]: At[ListMap[K, V], K, Option[V]] =
    At[ListMap[K, V], K, Option[V]] { k: K => lm: ListMap[K, V] =>
      lm.get(k)
    } { k: K => vOpt: Option[V] => lm: ListMap[K, V] =>
      vOpt.map(v => lm + (k -> v)).getOrElse(lm - k)
    }

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

  private def partitionHeadsLast[A](list: List[A]): (List[A], List[A]) =
    list.take(list.length - 1) -> list.drop(list.length - 1)

  private[dddblueprint] implicit def showListMap[K: ShowPretty, V: ShowPretty]: ShowPretty[ListMap[K, V]] =
    listMap => {
      val (mappedHeads, mappedLast) = partitionHeadsLast(
        listMap.toList
          .map {
            case (k, v) =>
              val (keyHeads, keyLast) = partitionHeadsLast(implicitly[ShowPretty[K]].showLines(k))
              val valueLines          = implicitly[ShowPretty[V]].showLines(v)

              keyHeads ++ keyLast.map(_ + " ->") ++ valueLines.map("  " + _)
          }
          .map(_.map("  " + _))
      )

      List("ListMap(") ++ mappedHeads.map(partitionHeadsLast).flatMap {
        case (heads, last) =>
          heads ++ last.map(_ + ",")
      } ++ mappedLast.flatten ++ List(")")
    }

  private[dddblueprint] implicit def showListSet[A: ShowPretty]: ShowPretty[ListSet[A]] =
    listSet => {
      val (mappedHeads, mappedLast) = partitionHeadsLast(listSet.toList.map { a =>
        implicitly[ShowPretty[A]].showLines(a).map("  " + _)
      })

      List("ListSet(") ++ mappedHeads.map(partitionHeadsLast).flatMap {
        case (heads, last) =>
          heads ++ last.map(_ + ",")
      } ++ mappedLast.flatten ++ List(")")
    }
}
