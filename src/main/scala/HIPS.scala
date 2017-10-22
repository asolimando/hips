import PntPackage.Pnt

import scala.collection.mutable.{ListBuffer, Map, TreeSet}
import scala.util.Random

import breeze.linalg._
import breeze.plot._

package object PntPackage {
  type Pnt = Point[Int, Double]
}

object HIPS {
  val VERBOSE: Boolean = false

  val pointOrderingX = Ordering.fromLessThan[Pnt](_.x < _.x)
  val pointOrderingXY = Ordering.fromLessThan[Pnt]((a,b) => if(a.x == b.x) a.y < b.y else a.x < b.x)
  val pointOrderingYX = Ordering.fromLessThan[Pnt]((a,b) => if(a.y == b.y) a.x < b.x else a.y < b.y)

  val XYtree = TreeSet.empty(pointOrderingXY)
  val YXtree = TreeSet.empty(pointOrderingYX)

  val rnd = new Random(10)

  def generatePoint(maxX: Int = Int.MaxValue,
                    maxY: Int = Int.MaxValue,
                    maxW: Double = Double.MaxValue): Pnt = {
    Point(rnd.nextInt(maxX), rnd.nextInt(maxY), (rnd.nextDouble * maxW).round)
  }

  def main(args: Array[String]): Unit = {
    for(i <- 1 to 1000) exec()
  }

  def exec(): Unit ={
    val points: Seq[Pnt] = (1 to 5).map(_ => generatePoint(100,100,10.0))
    /*    Seq(
          new Point(0, 1, 2.0),
          new Point(0, 0, 1.0),
          new Point(1, 0, 1.0),
          new Point(2, 1, 2.0)
        )
    */
    points.foreach(p => {XYtree.add(p); YXtree.add(p)})

    val M = new PointSet(XYtree)

    val solution = HIPS(M)

    val f1 = Figure()

    val p = f1.subplot(0)// += image(DenseMatrix.rand(200,200))

    p += plot(M.points.map(_.x).toSeq, M.points.map(_.y).toSeq)

    p += plot(solution.map(_.x), solution.map(_.y))

    p.refresh()

    val compWeight = solution.map(_.w).sum
    println("w = " + compWeight + " -> " + solution.toString)

    val expWeight = printSolution(points)

    if(compWeight != expWeight)
      throw new IllegalStateException("Weights are different: computed vs expected " + compWeight + " vs " + expWeight)
  }

  def printSolution[A <: Pnt](points: Seq[A]): Double = {
    val solutions = points.toSet.subsets
      .filter(increasingPointSet).toStream

    println(solutions.size + "/" + Math.pow(2, points.size).toInt + " increasing subsets")

    val solWeight = solutions.map(x => (x, x.map(_.w).sum))

    val maxWeight = solWeight.maxBy(_._2)

    println(solWeight.filter(_._2 == maxWeight._2).mkString("\n"))

    maxWeight._2
  }

  def increasingPointSet[A <: Pnt](ps: Set[A]): Boolean ={
    ps.forall(a => !ps.exists(b => b != a && a.x >= b.x && a.y <= b.y))
  }

  def HIPS(M: PointSet[Pnt]): List[Pnt] = {

    val P: Map[Pnt, Pnt] = Map()
    val S = new PointSet[Pnt](TreeSet.empty(pointOrderingYX))
    val AQ = ListBuffer[Pnt]()
    val SW: Map[Pnt, Double] = Map[Pnt, Double]()

    println(M.points.mkString(", "))

    var currXMaxW: Double = -1
    var i = 0
    var predX = M.points.head.x

    for(sigmai: Pnt <- M.points){

      if(predX != sigmai.x){
        processQueue(S, P, AQ, SW)
        currXMaxW = -1
        AQ.clear
      }

      i = i + 1
      var pred = S.predecessor(sigmai).getOrElse(null)

      SW.put(sigmai, SW.get(pred).getOrElse(0.0) + sigmai.w)

      P.put(sigmai, pred)

      if(SW.get(sigmai).get > currXMaxW){
        currXMaxW = SW.get(sigmai).get

        val succ = S.successor(pred)

        if(!succ.isDefined || sigmai.y < succ.get.y || SW.get(succ.get).get < SW.get(sigmai).get){
          AQ += sigmai
        }
      }

      if(i == M.size){
        processQueue(S, P, AQ, SW)
        currXMaxW = -1
        AQ.clear
      }
    }

    maximalSubset(SW, P)
  }

  def processQueue(S: PointSet[Pnt],
                   P: Map[Pnt, Pnt],
                   AQ: Seq[Pnt],
                   SW: Map[Pnt, Double]): Unit ={

    if(VERBOSE){
      println("S: " + S.points.mkString(", "))
      println("P: " + P.mkString(", "))
      println("AQ: " + AQ.mkString(", "))
      println("SW: " + SW.mkString(", "))
    }

    for (mu <- AQ) {
      var optSucc: Option[Pnt] = S.successor(P(mu))

      if(VERBOSE){
        println("mu: " + mu)
        println("succ: " + optSucc)
      }

      var break: Boolean = false

      while(optSucc.isDefined && !break){
        val succ: Pnt = optSucc.get

        if(SW.get(mu).get < SW.get(succ).get){
          break = true
        }
        else {
          S.remove(succ)
          optSucc = S.successor(succ)
        }
      }
    }

    AQ.foreach(S.add)

    if(VERBOSE){
      println("S: " + S.points.mkString(", "))
      println("AQ: " + AQ.mkString(", "))
    }
  }

  private def maximalSubset(SW: Map[Pnt, Double], P: Map[Pnt, Pnt]): List[Pnt] = _maximalSubset(P, List(SW.maxBy(_._2)._1))

  private def _maximalSubset(P: Map[Pnt, Pnt], MS: List[Pnt]): List[Pnt] = {
    val curr: Option[Pnt] = P.get(MS.head)
    val canContinue = curr.isDefined && curr.get != null
    if(canContinue) _maximalSubset(P, curr.get +: MS) else MS
  }
}

case class PointSet[A <: Pnt](points: TreeSet[A])(implicit cmp: Ordering[A] = points.ordering){

  /**
    * Returns the point having the greatest y component among those strictly lower than that of the current point, if any
    * @param curr the current point
    * @return the point having the greatest y component among those strictly lower than that of the current point, if any
    */
  def predecessor(curr: A): Option[A] = util.Try(points.filter(p => p.x < curr.x && p.y < curr.y).maxBy(_.y)).toOption

  /**
    * Returns the point having the lowest y component among those strictly greater than that of the current point, if any
    * @param curr the current point
    * @return the point having the lowest y component among those strictly greater than that of the current point, if any
    */
  def successor(curr: A): Option[A] = {
    //points.keysIteratorFrom(curr).min(cmp)
    util.Try(if(curr == null || curr.x < 0) points.head else points.filter(p => p.x > curr.x && p.y > curr.y).minBy(_.y)).toOption
  }

  /**
    * Removes the given element from the set.
    * @param p the element to remove.
    * @return true if the element was present in set, false otherwise.
    */
  def remove(p: A): Boolean = points.remove(p)

  /**
    * Adds the given element to the set.
    * @param p the element to add.
    * @return true if the element was not yet in the set, false otherwise.
    */
  def add(p: A): Boolean = points.add(p)

  /**
    * Returns the maximal element in the set.
    * @return the maximal element in the set.
    */
  def max(): A = points.max(cmp)

  /**
    * Returns the size of the set.
    * @return the size of the set.
    */
  def size(): Int = points.size

  override def toString(): String = points.mkString(", ")
}

case class Sequence[A <: Pnt](points: Seq[A]) extends Seq[A] {

  val weight: Double = points.map(_.w).sum

  override def length: Int = points.length

  override def iterator: Iterator[A] = points.iterator

  def apply(idx: Int): A = points.apply(idx)

  def dominates(that: Sequence[A]): Boolean ={
    val thisLast = this.last
    val thatLast = that.last

    (thatLast.y >= thisLast.y && thisLast.w > thatLast.w) || (thatLast.y > thisLast.y && thisLast.w >= thatLast.w)
  }
}

case class Point[A <% Comparable[A], B <% Comparable[B]](x: A, y: A, w: B) extends Ordered[Point[A,B]] {
  override def compare(that: Point[A, B]): Int = {
    if(this.x != that.x)
      this.x compareTo that.x
    else
      this.y compareTo that.y
  }
}
