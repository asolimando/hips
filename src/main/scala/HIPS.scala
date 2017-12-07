import PntPackage.Pnt

import scala.collection.mutable.{ListBuffer, Map, TreeSet}
import scala.util.Random
import breeze.plot._
import java.awt.Color

package object PntPackage {
  type Pnt = Point[Int, Double]
}

object HIPS {
  val VERBOSITY: Int = 1

  val pointOrderingX = Ordering.fromLessThan[Pnt](_.x < _.x)
  val pointOrderingXY = Ordering.fromLessThan[Pnt]((a,b) => if(a.x == b.x) a.y < b.y else a.x < b.x)
  val pointOrderingYX = Ordering.fromLessThan[Pnt]((a,b) => if(a.y == b.y) a.x < b.x else a.y < b.y)

  val rnd = new Random(2)

  var err = 0

  def generatePoint(maxX: Int = Int.MaxValue,
                    maxY: Int = Int.MaxValue,
                    maxW: Double = Double.MaxValue): Pnt = {
    Point(rnd.nextInt(maxX), rnd.nextInt(maxY), (rnd.nextDouble * maxW).round)
  }

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

  def main(args: Array[String]): Unit = {
    time{
      for(i <- 1 to 100000){ exec(); if(i > 358 || i % 10000 == 0) println(i)}
    }

    println(err)
  }

  def exec(): Unit ={
    val points: Seq[Pnt] = (1 to 5).map(_ => generatePoint(100,100,10.0))

    val XYtree = TreeSet.empty(pointOrderingXY)
    val YXtree = TreeSet.empty(pointOrderingYX)

    points.foreach(p => {XYtree.add(p); YXtree.add(p)})

    val M = new PointSet(XYtree)

    val compSol = HIPS(M)

    val f1 = Figure()

    val p = f1.subplot(0)// += image(DenseMatrix.rand(200,200))

    def weightFunc(s: collection.AbstractSeq[Pnt]): (Int => Double) = {
      ((x: Int) => s.map(_.w).zipWithIndex.map(_.swap).toMap.getOrElse(x, 0.0) / 2.0)
    }

    val compWeight = compSol.map(_.w).sum

    if(VERBOSITY > 0)
      println("w = " + compWeight + " -> " + compSol.toString)

    val (bfSol, bfWeight) = getBruteforceSolution(M.points.toSeq)
/*
    val allPoints = points.filterNot((compSol ++ bfSol).contains(_)).toList

    val all = scatter(
      allPoints.map(_.x),
      allPoints.map(_.y),
      weightFunc(allPoints),
      ((x: Int) => Color.BLACK)
    )

    p += all

    val common: Set[Pnt] = bfSol.intersect(compSol.toSet)
    val computed: Set[Pnt] = compSol.toSet.filterNot(common.contains(_))
    val bruteforce = bfSol.filterNot(common.contains(_))

    p += scatter(
      computed.map(_.x).toSeq,
      computed.map(_.y).toSeq,
      weightFunc(computed.toList),
      (x: Int) => Color.BLUE
    )

    p += scatter(
      bruteforce.map(_.x).toSeq,
      bruteforce.map(_.y).toSeq,
      weightFunc(bruteforce.toList),
      (x: Int) => Color.RED
    )

    p += scatter(
      common.map(_.x).toSeq,
      common.map(_.y).toSeq,
      weightFunc(common.toList),
      (x: Int) => Color.GREEN
    )

    p.refresh()
*/
    if(!increasingPointSet(compSol.toSet))
      throw new IllegalStateException("Illegal HIPS!")

    if(compWeight != bfWeight)
      //err += 1
      throw new IllegalStateException("Weights are different: computed vs expected " + compWeight + " vs " + bfWeight)
  }

  def getBruteforceSolution[A <: Pnt](points: Seq[A]): (Set[A], Double) = {

    val solutions = points.toSet.subsets.filter(s => increasingPointSet(s)).toStream

    if(VERBOSITY > 0)
      println(solutions.size + "/" + Math.pow(2, points.size).toInt + " increasing subsets")

    val solWeight = solutions.map(x => (x, x.toList.map(_.w).sum))

    val maxWeight = solWeight.maxBy(_._2)

    if(VERBOSITY > 0)
      println("bf --> w = " + maxWeight._2 + " -> " + maxWeight._1.mkString(", "))

    maxWeight
  }

  def increasingPointSet[A <: Pnt](ps: Set[A]): Boolean ={
    ps.forall(a => ps.filterNot(_ equals a).forall(b => (a.x > b.x && a.y > b.y) || (a.x < b.x && a.y < b.y)))
  }

  def HIPS(M: PointSet[Pnt]): List[Pnt] = {

    val P: Map[Pnt, Pnt] = Map()
    val S = new PointSet[Pnt](TreeSet.empty(pointOrderingYX))
    val AQ = ListBuffer[Pnt]()
    val SW: Map[Pnt, Double] = Map[Pnt, Double]()

    if(VERBOSITY > 0)
      println("Input: " + M.points.mkString(", "))

    var currXMaxW: Double = -1
    var i = 0

    for(sigmai: Pnt <- M.points){

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

      if(i == M.size || M.points.filterNot(_ equals sigmai).keysIteratorFrom(sigmai).next().x != sigmai.x){
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

    if(VERBOSITY > 1){
      println("S: " + S.points.mkString(", "))
      println("P: " + P.mkString(", "))
      println("AQ: " + AQ.mkString(", "))
      println("SW: " + SW.mkString(", "))
    }

    for (mu <- AQ) {
      var optSucc2: Option[Pnt] = S.successor(S.predecessor(mu).getOrElse(null)) // S.successor(P(mu))
      var optSucc: Option[Pnt] = S.successor(P(mu))

      if(VERBOSITY > 1){
        println("mu: " + mu)
        println("succ: " + optSucc)
      }

      var break: Boolean = false

      val currSeqWeight = SW.get(mu).get

      while(optSucc.isDefined && !break){
        val succ: Pnt = optSucc.get

        if(currSeqWeight < SW.get(succ).get){
          break = true
        }
        else {
          S.remove(succ)
          optSucc = S.successor(P(mu)) //S.successor(succ)
        }
      }
    }

    AQ.foreach(S.add)

    if(VERBOSITY > 1){
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
  def predecessor(curr: A): Option[A] = {
    util.Try({
      val comparator = points.ordering
      val a = points.iterator.takeWhile(comparator.lt(_, curr)).max(comparator)
      val b = points.filter(p => p.x < curr.x && p.y < curr.y).maxBy(_.y)
      if(a != b) {
        //System.err.println(a + " " + b)
//        System.exit(-1)
      }
      b
    }).toOption
  }

  /**
    * Returns the point having the lowest y component among those strictly greater than that of the current point, if any
    * @param curr the current point
    * @return the point having the lowest y component among those strictly greater than that of the current point, if any
    */
  def successor(curr: A): Option[A] = {
    util.Try(
      if(curr == null || curr.x < 0)
        points.head
      else
        points.filterNot(_ equals curr)
          .keysIteratorFrom(curr)
          .min(points.ordering)
    ).toOption
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
