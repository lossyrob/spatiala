package spatiala.index

import org.khelekore.prtree._
import java.awt.geom.Rectangle2D
import java.util.Collections

import spatiala.Extent

import scala.collection.mutable
import scala.collection.JavaConversions._

object KheleIndex {
  def apply[T](points:Iterable[T])(f:T=>(Double,Double)):KheleIndex[T] = {
    val si = new KheleIndex[T](Measure.Dumb)(f)
    si.load(points)
    si
  }
}

class KheleIndex[T](val measure:Measure)(f:T=>(Double,Double)) extends SpatialIndex[T] {
  val rtree = new PRTree[Rectangle2D](new Rectangle2DConverter(),30)
  val dc = new RectDistance()
  val acceptAll = new AcceptAll[Rectangle2D]()
  val points = mutable.Map[(Double,Double),T]()

  def load(vs:Iterable[T]) = {
    val rects = vs.map(f).map(t => new Rectangle2D.Double(t._1,t._2,0,0))
    rtree.load(rects)
    for(v <- vs) { points(f(v)) = v }
  }

  def nearest(x:Double,y:Double):T = {
    val r = rtree.nearestNeighbour(dc,acceptAll, 10, new SimplePointND(x,y)).get(0).get()
    points((r.getMinX,r.getMinY))
  }
}

class Rectangle2DConverter extends MBRConverter[Rectangle2D] {
  def getDimensions() = 2

  def getMin(axis:Int, t:Rectangle2D) =
    if(axis == 0) {
      t.getMinX 
    } else {
      t.getMinY ()
    }

  def getMax(axis:Int, t:Rectangle2D) =
    if(axis == 0) {
      t.getMaxX 
    } else {
      t.getMaxY ()
    }
}

class RectDistance extends DistanceCalculator[Rectangle2D] {
  def distanceTo (r:Rectangle2D, p:PointND) = {
    val md = MinDist2D.get (r.getMinX, r.getMinY,
                            r.getMaxX, r.getMaxY,
                            p.getOrd(0), p.getOrd(1))
    math.sqrt(md)
  }
}

class AcceptAll[T] extends NodeFilter[T] {
  def accept (t:T) = true
}
