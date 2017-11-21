package org.asolimando.viz

import org.asolimando.point.pntpackage.Pnt

import breeze.plot._
import java.awt.Color

case class SolutionViz[A <: Pnt](points: Seq[A], solution: Seq[A]) {

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
