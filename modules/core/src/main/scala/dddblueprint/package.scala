package object dddblueprint extends TypeClasses with Conversions {

  type ListMap[K, V] = scala.collection.immutable.ListMap[K, V]
  val ListMap = scala.collection.immutable.ListMap

  type ListSet[A] = scala.collection.immutable.ListSet[A]
  val ListSet = scala.collection.immutable.ListSet
}
