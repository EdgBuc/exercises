package exercises.balanced.tree

trait LookupIndex[V] {
  def contains(value: V): Boolean
  def withValue(value: V): LookupIndex[V]
}
