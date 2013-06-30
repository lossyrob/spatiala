package spatiala

case class Extent(xmin:Double,ymin:Double,xmax:Double,ymax:Double) {
  val width = (xmax-xmin)
  val height = (ymax-ymin)
  val area = width*height

  def intersects(other:Extent):Boolean = 
    (xmin < other.xmax) ||
    (other.xmin < xmax) ||
    (ymin < other.ymax) ||
    (other.xmin < ymax)

  def <(other:Extent) = 
    this.area < other.area

  def >(other:Extent) = 
    this.area > other.area

  def compare(other:Extent) =
    this.area.compare(other.area)

  def areaIncreaseToFit(other:Extent):Double = {
    val dx = 
      (math.max(0,xmin - other.xmin) +
       math.max(0,other.xmax - xmax))
    val dy = 
      (math.max(0,ymin - other.ymin) +
       math.max(0,other.ymax - ymax))
    (dx*height) + (dy * (dx+width))    
  }

  def expandToFit(other:Extent) =
    Extent(math.min(xmin,other.xmin),
           math.min(ymin,other.ymin),
           math.max(xmax,other.xmax),
           math.max(ymax,other.ymax))
}
