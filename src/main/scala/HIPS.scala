package org.asolimando.hips

import org.asolimando.point._
import org.asolimando.point.pntpackage.Pnt
import org.asolimando.viz.SolutionViz

import scala.collection.mutable.{ListBuffer, Map, TreeSet}
import scala.util.Random

/**
  * Heaviest Increasing Point Subset object.
  */
object HIPS {

  val VERBOSITY: Int = 1
  val rnd: Random = new Random(2)

  /**
    * Method generating a point (x, y, w).
    * @param maxX x < maxX
    * @param maxY y < maxY
    * @param maxW w < maxW
    * @return a point.
    */
  def generatePoint(maxX: Int = Int.MaxValue,
                    maxY: Int = Int.MaxValue,
                    maxW: Double = Double.MaxValue): Pnt = {
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

  def main(args: Array[String]): Unit = {
    time{
      for(i <- 1 to 10){
        val points: Seq[Pnt] = (1 to 1500).map(_ => HIPS.generatePoint(1000,1000,100.0))
        val solution = exec(points)
        if(i % 10000 == 0) println(i)

        SolutionViz(points, solution).visualize
      }
    }
  }

  /**
    * Computes a HIPS from a given set of points.
    * @param points the set of points for which the HIPS is sought
    * @param bruteforceCheck true if we want to check the computed solution against the one computed using a bruteforce method.
    * @return a HIPS from a given set of points.
    */
  def exec(points: Seq[Pnt], bruteforceCheck: Boolean = false): List[Pnt] ={

    val pointOrderingXY = Ordering.fromLessThan[Pnt]((a,b) => if(a.x == b.x) a.y < b.y else a.x < b.x)

    val XYtree = TreeSet.empty(pointOrderingXY)

    points.foreach(XYtree.add(_))

    val M = new PointSet(XYtree)

    val compSol = computeHIPS(M)

    val compWeight = compSol.map(_.w).sum

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
  private def getBruteforceSolution(points: Seq[Pnt]): (Set[Pnt], Double) = {

    val solutions = points.toSet.subsets.filter(s => isStrictlyIncreasingPointSet(s)).toStream

    if(VERBOSITY > 0)
      println(solutions.size + "/" + Math.pow(2, points.size).toInt + " increasing subsets")

    val solWeight = solutions.map(x => (x, x.toList.map(_.w).sum))

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
    ps.forall(a => ps.filterNot(_ equals a).forall(b => (a.x > b.x && a.y > b.y) || (a.x < b.x && a.y < b.y)))
  }

  /**
    * Computes one of the HIPS of the given set of points.
    * @param M the given set of points
    * @return one of the HIPS of the given set of points.
    */
  def computeHIPS(M: PointSet[Pnt]): List[Pnt] = {

    val pointOrderingYX = Ordering.fromLessThan[Pnt]((a,b) => if(a.y == b.y) a.x < b.x else a.y < b.y)

    val P = Map[Pnt, Pnt]()
    val S = new PointSet(TreeSet.empty(pointOrderingYX))
    val AQ = ListBuffer[Pnt]()
    val SW = Map[Pnt, Double]()

    if(VERBOSITY > 0)
      println("Input: " + M.points.mkString(", "))

    var currXMaxW: Double = -1
    var i = 0

    for(sigmai <- M.points){

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

  /**
    * Process the addition queue of points waiting to be added to the partial solution set.
    * @param S set composed by the ending elements of each non-dominated partial solution
    * @param P the predecessor map for each element
    * @param AQ the addition queue to be processed
    * @param SW the (total) weight of each non-dominated partial solution
    */
  private def processQueue(S: PointSet[Pnt],
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
  private def maximalSubset(SW: Map[Pnt, Double], P: Map[Pnt, Pnt]): List[Pnt] =
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