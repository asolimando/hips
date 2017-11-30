package com.github.asolimando.viz

import java.awt.Color

import breeze.plot._
import com.github.asolimando.point.{Monoid, Point}

/**
  * A visualization class for HIPS over the input set of points.
  * @param points the input set of points
  * @param solution the HIPS for the input set of points
  * @tparam A the point type parameter
  */
case class SolutionViz[A:Ordering, B:Monoid:Ordering](points: Seq[Point[A,B]], solution: Seq[Point[A,B]])
                                                     (implicit convA:A => Int, convB:B => Double){

  type Pnt = Point[A,B]
  private val mB = implicitly[Monoid[B]]

  /**
    * Method displaying the HIPS over the input set of points
    */
  def visualize(): Unit = {

    def weightFunc(s: collection.AbstractSeq[Pnt]): (Int => Double) = {
      ((x: Int) => convB(s.map(_.w).zipWithIndex.map(_.swap).toMap.getOrElse(x, mB.zero)) / 5.0)
    }

    val f1 = Figure()

    val p = f1.subplot(0)

    val allPoints = points.filterNot(solution.contains(_)).toList

    p += scatter(
      solution.map(p => convA(p.x)),
      solution.map(p => convA(p.y)),
      weightFunc(solution.toList),
      (x: Int) => Color.RED)

    p += scatter(
      allPoints.map(p => convA(p.x)),
      allPoints.map(p => convA(p.y)),
      weightFunc(allPoints),
      (x: Int) => Color.BLUE)

    p.refresh()
  }
}
