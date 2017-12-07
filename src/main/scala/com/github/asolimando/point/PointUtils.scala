package com.github.asolimando.point

import com.github.asolimando.point.pntpackage.Pnt

import scala.collection.mutable.TreeSet

package object pntpackage {
  type Pnt = Point[Int, Double]
}

/**
  * A sequence of points.
  * @param points the points composing the sequence.
  * @tparam A the point type parameter.
  */
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

/**
  * A 2D weighted point.
  * @param x the x component
  * @param y the y component
  * @param w the weight
  * @tparam A the type parameter for the x and y components
  * @tparam B the type parameter for the weight component
  */
case class Point[A <% Comparable[A], B <% Comparable[B]](x: A, y: A, w: B) extends Ordered[Point[A, B]] {
  override def compare(that: Point[A, B]): Int = { //default XY comparator
    if(this.x != that.x)
      this.x compareTo that.x
    else
      this.y compareTo that.y
  }
}

/**
  * A set of points.
  * @param points the points composing the set.
  * @param cmp the comparator for ordering the points.
  * @tparam A the point type parameter.
  */
case class PointSet[A <: Pnt](points: TreeSet[A])(implicit cmp: Ordering[A] = points.ordering){

  /**
    * Returns the point having the greatest y component among those strictly lower than that of the current point, if any
    * @param curr the current point
    * @return the point having the greatest y component among those strictly lower than that of the current point, if any
    */
  def predecessor(curr: A): Option[A] = {
    util.Try(points.filter(p => p.x < curr.x && p.y < curr.y).maxBy(_.y)).toOption
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