package spatiala

import spatiala.index._

import com.vividsolutions.jts.index.strtree.STRtree
import com.vividsolutions.jts.geom.Coordinate
import com.vividsolutions.jts.geom.Envelope

import spire.syntax._

case class Point(x:Double, y:Double) {
  def envelope:Envelope = {
    new Envelope(new Coordinate(x,y))
  }

  override
  def toString = s"Point($x,$y)"

  def toTuple:(Double,Double) = (x,y)
}

object Main {
  def main(args:Array[String]) = {
    val nPoints = 2000000
    val nTests = 1000*1000

    val extent = Extent(-8376428.180493358, 4847676.906022543,-8355331.560689615,4867017.75944691)

    Logger.log(" ---- Creating Test Data ----")
    val points =
      Logger.timedCreate(s"Generating $nPoints points...") {
        PointGenerator.generate(nPoints, extent)
      }

    // Create indices 
    Logger.log(" ---- Generating Indicies ----")
    val jtsIndex = 
      Logger.timedCreate("Creating JTS index.") {
        JtsIndex(points)(_.toTuple)
      }

    // val jsiIndex = 
    //   Logger.timedCreate("Creating JSI index.") {
    //     JsiIndex(points)(_.toTuple)
    //   }

    // val kheleIndex = 
    //   Logger.timedCreate("Creating Khele index.") {
    //     KheleIndex(points)(_.toTuple)
    //   }

    // Do benchmark
    val tps = (for(i <- 0 until nTests) yield { PointGenerator.generate(extent) }).toArray 
    val exts = 
      tps.map { p => Extent(p.x - 150, p.y - 150, p.x + 150, p.y + 150) }
         .toArray

    Logger.log(" ---- Running Test ----")
    Logger.log("   -- JTS --")
    Logger.timed(s"    Getting closest points for $nTests random points.","    Finished.") {
      cfor(0)(_ < nTests, _ + 1) { i =>
        val p = tps(i)
        jtsIndex.nearest(p.x,p.y)
      }
    }

    // Logger.log("   -- JSI --")
    // Logger.timed(s"    Getting closest points for $nTests random points.","    Finished.") {
    //   cfor(0)(_ < nTests, _ + 1) { i =>
    //     val p = tps(i)
    //     jsiIndex.nearest(p.x,p.y)
    //   }
    // }

    // Logger.log("   -- Khele --")
    // Logger.timed(s"    Getting closest points for $nTests random points.","    Finished.") {
    //   cfor(0)(_ < nTests, _ + 1) { i =>
    //     val p = tps(i)
    //     kheleIndex.nearest(p.x,p.y)
    //   }
    // }

    Logger.log("   -- JTS --")
    Logger.timed(s"    Getting points in envolope close to $nTests random points.","    Finished.") {
      cfor(0)(_ < nTests, _ + 1) { i =>
        val e = exts(i)
        jtsIndex.pointsInExtent(e)
      }
    }

    // Logger.log("   -- JSI --")
    // Logger.timed(s"    Getting points in envolope close to $nTests random points.","    Finished.") {
    //   cfor(0)(_ < nTests, _ + 1) { i =>
    //     val p = exts(i)
    //     jsiIndex.nearest(p.x,p.y)
    //   }
    // }

    // Logger.log("   -- Khele --")
    // Logger.timed(s"    Getting points in envolope close to $nTests random points.","    Finished.") {
    //   cfor(0)(_ < nTests, _ + 1) { i =>
    //     val p = exts(i)
    //     kheleIndex.nearest(p.x,p.y)
    //   }
    // }

    Logger.log(" ---- Test complete. ----")
  }
}

import scala.util.Random

object PointGenerator {
  def generate(n:Int, extent:Extent) = {
    val dx = extent.xmax - extent.xmin
    val dy = extent.ymax - extent.ymin
    for(i <- 0 until n) yield {
      Point(extent.xmin + (Random.nextDouble * dx), extent.ymin + (Random.nextDouble * dy))
    }
  }

  def generate(extent:Extent) = {
    val dx = extent.xmax - extent.xmin
    val dy = extent.ymax - extent.ymin

    Point(extent.xmin + (Random.nextDouble * dx), extent.ymin + (Random.nextDouble * dy))
  }
}
