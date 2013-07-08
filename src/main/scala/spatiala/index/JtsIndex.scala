package spatiala.index

import spatiala.Extent

import com.vividsolutions.jts.index.strtree.{STRtree, ItemDistance, ItemBoundable}
import com.vividsolutions.jts.index.strtree.ItemDistance
import com.vividsolutions.jts.geom.Coordinate
import com.vividsolutions.jts.geom.Envelope

import scala.collection.mutable
import scala.collection.JavaConversions._

object JtsIndex {
  def apply[T](values:Iterable[T])(f:T=>(Double,Double)):JtsIndex[T] = {
    val si = new JtsIndex[T](Measure.Dumb)(f)
    si.load(values)
    si
  }
}

class JtsIndex[T](val measure:Measure)(f:T=>(Double,Double)) extends SpatialIndex[T] {
  val rtree = new STRtree
  val points = mutable.Set[T]()

  def load(vs:Iterable[T]) = {
    for(v <- vs) {
      insert(v)
    }
  }

  def insert(v:T) = {
    val (x,y) = f(v)
    rtree.insert(new Envelope(new Coordinate(x,y)), v)
    points.add(v)
  }

  def nearest(x:Double,y:Double):T = {
    rtree.nearestNeighbour(new Envelope(new Coordinate(x,y)),null,measure).asInstanceOf[T]
  }

  def pointsInExtent(extent:Extent):Seq[T] = {
    rtree.query(new Envelope(extent.ymin,extent.ymax,extent.xmin,extent.xmax))
         .map(_.asInstanceOf[T])
  }
}

object Measure {
  def Dumb = new DumbMeasure
}

trait Measure extends ItemDistance with Serializable {
  def distance(x1:Double,y1:Double,x2:Double,y2:Double):Double

  def distance(i1:ItemBoundable, i2:ItemBoundable):Double = {
    val bound1 = i1.getBounds.asInstanceOf[Envelope]
    val bound2 = i2.getBounds.asInstanceOf[Envelope]
    distance(bound1.getMinX,bound1.getMinY,bound2.getMinX,bound2.getMinY)
  }
}

class DumbMeasure() extends Measure {
  def distance(x1:Double,y1:Double,x2:Double,y2:Double):Double = {
    val x = x2 - x1
    val y = y2 - y1
    math.sqrt(x*x + y*y)
  }
}
