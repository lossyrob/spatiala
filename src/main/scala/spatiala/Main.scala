package spatiala

import scala.collection.mutable
import scala.annotation.tailrec

object PRTree {
  def main(args:Array[String]) = {

  }
}

abstract class Node(val initialExtent:Extent) {
  private var _extent = initialExtent
  def extent = _extent

  def count:Int
  val isLeaf:Boolean

  def expandToFit(newExtent:Extent) = {
    _extent = _extent.expandToFit(newExtent)
  }
}

trait NodeContainer[T] {
  private val containerNodes = mutable.ListBuffer[ContainerNode[T]]()
  private val leafNodes = mutable.ListBuffer[LeafNode[T]]()

  private var _count = 0
  def count = _count

  def +=(n:ContainerNode[T]) = {
    containerNodes += n
    _count += 1
  }

  def +=(n:LeafNode[T]) = {
    leafNodes += n
  }

  def foreachContainer(e:Extent)(f:ContainerNode[T]=>Unit) = 
    containerNodes.foreach(f)

  def minToFit(extent:Extent):Node = {
    val nodes = (containerNodes ++ leafNodes).toList
    if(nodes.isEmpty) { 
      sys.error("This node container doesn't contain any nodes.") 
    }

    (nodes
      .foldLeft((nodes(0),nodes(0).extent.areaIncreaseToFit(extent))) {
      (minNode,node) =>
        val a1 = minNode._2
        val a2 = node.extent.areaIncreaseToFit(extent)
        if(a1 < a2) { 
          minNode 
        } else {
          if(a1 > a2) {
            (node,a2) 
          } else {
            //tie. take min area
            val ea1 = minNode._1.extent.area 
            val ea2 = node.extent.area
            if(ea1 < ea2) {
              minNode
            } else if(ea1 > ea2) {
              (node,a2)
            } else {
              if(minNode._1.count <= node.count) {
                minNode
              } else {
                (node,a2)
              }
            }
          }
        }
    })._1
  }
}

case class Entry[T](extent:Extent,value:T)

object LeafNode {
  def apply[T](extent:Extent,value:T) = {
    val l = new LeafNode[T](extent)
    l += Entry(extent,value)
    l
  }
}

class LeafNode[T](extent:Extent) extends Node(extent) {
  private val _entries = mutable.ListBuffer[Entry[T]]()
  private var _count = 0

  val isLeaf = true

  def +=(entry:Entry[T]):Unit = {
    _entries += entry
    _count += 1
    expandToFit(entry.extent)
  }

  def count = _count
}

class ContainerNode[T](extent:Extent) extends Node(extent) with NodeContainer[T]  {
  val isLeaf = false
}

class RootNode[T] extends NodeContainer[T]

/*
 * Basic R Tree implementation.
 * 
 * Follows rules of R Tree:
 * 1. minNodes <= M/2
 * 2. Each leaf node (unless it is root) can host up to maxNodes 
 *    entries, and must host at least minNodes entries.
 */
class RTree[T](minNodes:Int,maxNodes:Int) {
  // Make sure we're following rule 1
  if(minNodes > (maxNodes/2)) {
    sys.error("minNodes must be <= to maxNodes")
  }

  var initialRoot:LeafNode[T] = null
  val root = new RootNode[T]
  var useRoot = false

  def insert(extent:Extent,obj:T) = {
    if(!useRoot) {
      if(initialRoot == null) {
        initialRoot = LeafNode(extent,obj)
      } else {
        initialRoot += Entry(extent,obj)
      }
    } else {
      @tailrec
      def insertRec(node:NodeContainer[T]):Unit = {
        val min = node.minToFit(extent)
        if(min.isLeaf) {
          val leaf = min.asInstanceOf[LeafNode[T]]
          val entry = Entry(extent,obj)
          if(leaf.count < maxNodes) {
            leaf += entry
          } else {
            //Split the leaf into two leaves l1 and l2, based on split algorithm
            //Assign the rest of the edges to either l1 or l2, depending on minimum
            //  area increase to fit the entrie's extent.
            //If E is the number of entries left to be assigned, and on of the
            //  leaves contains minNodes - E entries, assign the remaining entries 
            //  to that leaf.
            val (l1,l2) = LeafSplit.quadratic(leaf)
          }
          return
        } else {
          min.expandToFit(extent)
          insertRec(min.asInstanceOf[ContainerNode[T]])
        }
      }

      insertRec(root)
    }
  }
}

object LeafSplit {
  def quadratic[T](leaf:LeafNode[T]) = {
    //Maximal dead space
    //TODO: Implement
    (leaf,leaf)
  }
}
