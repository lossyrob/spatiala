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

  def distanceTo(other:Extent) = {
    val dx = 
      if(xmax < other.xmin) { other.xmin - xmax }
      else if(xmin > other.xmax) { xmin - other.xmax }
      else { 0 }

    val dy = 
      if(ymax < other.ymin) { other.ymin - ymax }
      else if(ymin > other.ymax) { ymin - other.ymax }
      else { 0 }

    math.sqrt(dx*dx+dy*dy)
  }

  def deadSpaceWith(other:Extent) = {
    var uXmin = 0.0
    var uXmax = 0.0
    var uYmin = 0.0
    var uYmax = 0.0
    var iXmin = 0.0
    var iXmax = 0.0
    var iYmin = 0.0
    var iYmax = 0.0

    if(xmin < other.xmin) { uXmin = xmin ; iXmin = other.xmin }
    else { uXmin = other.xmin ; iXmin = xmin }

    if(xmax < other.xmax) { uXmax = other.xmax ; iXmax = xmax }
    else { uXmax = xmax ; iXmax = other.xmax }

    if(ymin < other.ymin) { uYmin = ymin ; iYmin = other.ymin }
    else { uYmin = other.ymin ; iYmin = ymin }

    if(ymax < other.ymax) { uYmax = other.ymax ; iYmax = ymax }
    else { uYmax = ymax ; iYmax = other.ymax }

    (uXmax - uXmin) * (uYmax - uYmin) -
    height * width -
    other.height*other.width +
    (iXmax - iXmin) * (iYmax - iYmin)
  }
}
