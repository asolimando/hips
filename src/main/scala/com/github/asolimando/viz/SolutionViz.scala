package com.github.asolimando.viz

import java.awt.Color

import breeze.plot._
import com.github.asolimando.point.pntpackage.Pnt

/**
  * A visualization class for HIPS over the input set of points.
  * @param points the input set of points
  * @param solution the HIPS for the input set of points
  * @tparam A the point type parameter
  */
case class SolutionViz[A <: Pnt](points: Seq[A], solution: Seq[A]) {

  /**
    * Method displaying the HIPS over the input set of points
    */
  def visualize(): Unit = {

    def weightFunc(s: collection.AbstractSeq[A]): (Int => Double) = {
      ((x: Int) => s.map(_.w).zipWithIndex.map(_.swap).toMap.getOrElse(x, 0.0) / 5.0)
    }

    val f1 = Figure()

    val p = f1.subplot(0)

    val allPoints = points.filterNot(solution.contains(_)).toList

    p += scatter(
      solution.map(_.x),
      solution.map(_.y),
      weightFunc(solution.toList),
      (x: Int) => Color.RED
    )

    p += scatter(
      allPoints.map(_.x),
      allPoints.map(_.y),
      weightFunc(allPoints),
      (x: Int) => Color.BLUE
    )

    p.refresh()
  }

}
