package spatiala.index

trait SpatialIndex[T] {
  def load(vs:Iterable[T]):Unit
}
