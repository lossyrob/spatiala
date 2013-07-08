package spatiala.index

import spatiala.Extent

import com.infomatiq.jsi

import gnu.trove._

import scala.collection.mutable
import scala.collection.JavaConversions._

object JsiIndex {
  def apply[T](values:Iterable[T])(f:T=>(Double,Double)):JsiIndex[T] = {
    val si = new JsiIndex[T](Measure.Dumb)(f)
    si.load(values)
    si
  }
}

class JsiIndex[T](val measure:Measure)(f:T=>(Double,Double)) extends SpatialIndex[T] {
  val rtree = new jsi.rtree.RTree
  rtree.init(null)

  val points = mutable.ListBuffer[T]()

  def load(vs:Iterable[T]) = {
    for(v <- vs) {
      insert(v)
    }
  }

  def insert(v:T) = {
    val (x,y) = f(v)
    val id = points.length
    rtree.add(new jsi.Rectangle(x.toFloat,y.toFloat,x.toFloat,y.toFloat), id)
    points += v
  }

  def nearest(x:Double,y:Double):T = {
    var result = -1
    rtree.nearestN(new jsi.Point(x.toFloat,y.toFloat), new TIntProcedure() {
      def execute(i:Int) = {
        result = i
        true
      }
    }, 3, java.lang.Float.MAX_VALUE)
    points(result)
  }
}
