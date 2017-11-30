package com.github.asolimando.hips

import com.github.asolimando.point._
import com.github.asolimando.point.pntpackage.PntID
import com.github.asolimando.viz.SolutionViz

import scala.collection.mutable.{ListBuffer, Map, TreeSet}
import scala.util.Random

object HIPS {
  /**
    * Method generating a point (x, y, w).
    * @param maxX x < maxX
    * @param maxY y < maxY
    * @param maxW w < maxW
    * @return a point.
    */
  def generatePoint(maxX: Int = Int.MaxValue,
                    maxY: Int = Int.MaxValue,
                    maxW: Double = Double.MaxValue,
                    rnd: Random): PntID = {
    Point(rnd.nextInt(maxX), rnd.nextInt(maxY), (rnd.nextDouble * maxW).round)
  }

  /**
    * Timing function
    * @param a a function
    * @tparam B the return type of the function
    * @return the result of the function after printing its execution time
    */
  def time[B](a: => B) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

  def apply[A: Ordering, B: Ordering:Monoid]: HIPS[A,B] = new HIPS[A, B]() {}

  val hipsPnt = HIPS[Int, Double]

  def main(args: Array[String]): Unit = {
    time{
      val rnd = new Random(2)

      (1 to 10).foreach{i =>
        val points = (1 to 1500).map(_ => generatePoint(1000, 1000, 100.0, rnd))
        val solution = hipsPnt.exec(points)
        if(i % 10000 == 0) println(i)

        SolutionViz(points, solution).visualize
      }
    }
  }
}

/**
  * Heaviest Increasing Point Subset object.
  */
abstract class HIPS[A:Ordering, B:Monoid:Ordering] {

  type Pnt = Point[A,B]

  val VERBOSITY: Int = 1

  private val mB = implicitly[Monoid[B]]
  private val oB = implicitly[Ordering[B]]
  private val oA = implicitly[Ordering[A]]

  /**
    * Computes a HIPS from a given set of points.
    * @param points the set of points for which the HIPS is sought
    * @param bruteforceCheck true if we want to check the computed solution against the one computed using a bruteforce method.
    * @return a HIPS from a given set of points.
    */
  def exec(points: Seq[Pnt], bruteforceCheck: Boolean = false): List[Pnt] ={
    val pointOrderingXY = implicitly[Ordering[(A,A)]]

    val XYtree = TreeSet.empty[Pnt](pointOrderingXY.on(p => (p.x, p.y)))

    points.foreach(XYtree.add)

    val M = PointSet(XYtree)

    val compSol = computeHIPS(M)

    val compWeight = compSol.map(_.w).fold(mB.zero)(mB.op)

    if(VERBOSITY > 0)
      println("w = " + compWeight + " -> " + compSol.toString)

    if(!isStrictlyIncreasingPointSet(compSol.toSet))
      throw new IllegalStateException("Illegal HIPS!")

    if(bruteforceCheck){
      val (bfSol, bfWeight) = getBruteforceSolution(M.points.toSeq)

      if(compWeight != bfWeight)
        throw new IllegalStateException("Weights are different: computed vs expected " + compWeight + " vs " + bfWeight)
    }

    compSol
  }

  /**
    * Computes a HIPS using a bruteforce approach
    * @param points the set of points for which the HIPS is sought
    * @return a HIPS using a bruteforce approach
    */
  private def getBruteforceSolution(points: Seq[Pnt]): (Set[Pnt], B) = {

    val solutions = points.toSet.subsets.filter(s => isStrictlyIncreasingPointSet(s)).toStream

    if(VERBOSITY > 0)
      println(solutions.size + "/" + Math.pow(2, points.size).toInt + " increasing subsets")

    val solWeight = solutions.map(x => (x, x.toList.map(_.w).fold(mB.zero)(mB.op)))

    val maxWeight = solWeight.maxBy(_._2)

    if(VERBOSITY > 0)
      println("bf --> w = " + maxWeight._2 + " -> " + maxWeight._1.mkString(", "))

    maxWeight
  }

  /**
    * Tests if the set of points is stricly increasing or not.
    * @param ps the point set to test
    * @return true if the set of points is stricly increasing, false otherwise.
    */
  private def isStrictlyIncreasingPointSet(ps: Set[Pnt]): Boolean ={
    ps.forall(a => ps.filterNot(_ equals a).forall{
      b => (oA.gt(a.x, b.x) && oA.gt(a.y, b.y)) || (oA.lt(a.x, b.x) && oA.lt(a.y, b.y))
    })
  }

  /**
    * Computes one of the HIPS of the given set of points.
    * @param M the given set of points
    * @return one of the HIPS of the given set of points.
    */
  def computeHIPS(M: PointSet[A, B]): List[Pnt] = {

    val pointOrderingYX:Ordering[Pnt] = implicitly[Ordering[(A,A)]].on(p => (p.y, p.x))

    val S = PointSet[A,B](TreeSet.empty(pointOrderingYX))
    val P = Map[Pnt, Pnt]()
    val AQ = ListBuffer[Pnt]()
    val SW = Map[Pnt, B]()

    if(VERBOSITY > 0)
      println("Input: " + M.points.mkString(", "))

    var currXMaxW: B = mB.zero
    var i = 0

    for(sigmai <- M.points){

      i = i + 1
      var pred = S.predecessor(sigmai).getOrElse(null)

      SW.put(sigmai, mB.op(SW.getOrElse(pred, mB.zero), sigmai.w))

      P.put(sigmai, pred)

      if(oB.gteq(SW(sigmai),currXMaxW)){
        currXMaxW = SW(sigmai)

        val succ = S.successor(pred)

        if(succ.isEmpty || oA.lt(sigmai.y,succ.get.y) || oB.lt(SW(succ.get),SW(sigmai))) {
          AQ += sigmai
        }
      }

      if(i == M.size || M.points.filterNot(_ equals sigmai).keysIteratorFrom(sigmai).next().x != sigmai.x){
        processQueue(S, P, AQ, SW)
        currXMaxW = mB.zero
        AQ.clear
      }
    }

    maximalSubset(SW, P)
  }

  /**
    * Process the addition queue of points waiting to be added to the partial solution set.
    * @param S set composed by the ending elements of each non-dominated partial solution
    * @param P the predecessor map for each element
    * @param AQ the addition queue to be processed
    * @param SW the (total) weight of each non-dominated partial solution
    */
  private def processQueue(S: PointSet[A,B],
                           P: Map[Pnt, Pnt],
                           AQ: Seq[Pnt],
                           SW: Map[Pnt, B]): Unit ={

    if(VERBOSITY > 1){
      println("S: " + S.points.mkString(", "))
      println("P: " + P.mkString(", "))
      println("AQ: " + AQ.mkString(", "))
      println("SW: " + SW.mkString(", "))
    }

    for (mu <- AQ) {
      var optSucc: Option[Pnt] = S.successor(P(mu))

      if(VERBOSITY > 1){
        println("mu: " + mu)
        println("succ: " + optSucc)
      }

      var break: Boolean = false

      val currSeqWeight = SW.get(mu).get

      while(optSucc.isDefined && !break){
        val succ: Pnt = optSucc.get

        if(oB.lt(currSeqWeight, SW.get(succ).get)){
          break = true
        }
        else {
          S.remove(succ)
          optSucc = S.successor(P(mu))
        }
      }
    }

    AQ.foreach(S.add)

    if(VERBOSITY > 1){
      println("S: " + S.points.mkString(", "))
      println("AQ: " + AQ.mkString(", "))
    }
  }

  /**
    * Identifies (one of) the maximal subset among the non-dominated solutions
    * @param SW the (total) weight of each non-dominated partial solution
    * @param P the predecessor map for each element
    * @return (one of) the maximal subset among the non-dominated solutions
    */
  private def maximalSubset(SW: Map[Pnt, B], P: Map[Pnt, Pnt]): List[Pnt] =
    _maximalSubset(P, List(SW.maxBy(_._2)._1))

  /**
    * Auxiliary method accumulating (one of) the maximal subset among the non-dominated solutions
    * @param P the predecessor map for each element
    * @param MS the maximal subset if its head has no predecessor,
    *           the partial maximal subset complete from its head to the end otherwise
    * @return (one of) the maximal subset among the non-dominated solutions
    */
  private def _maximalSubset(P: Map[Pnt, Pnt], MS: List[Pnt]): List[Pnt] = {
    val curr: Option[Pnt] = P.get(MS.head)
    val canContinue = curr.isDefined && curr.get != null
    if(canContinue) _maximalSubset(P, curr.get +: MS) else MS
  }
}