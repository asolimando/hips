package com.github.asolimando.point

import scala.collection.mutable.TreeSet

package object pntpackage {
  type PntID = Point[Int, Double]
}

trait Monoid[T] {
  def op(t1: T, t2: T): T
  def zero: T
}

object Monoid {
  implicit val dblMonoid:Monoid[Double] = new Monoid[Double] {
    override def op(t1: Double, t2: Double): Double = t1 + t2
    override def zero: Double = 0D
  }
}

/**
  * A sequence of points.
  * @param points the points composing the sequence.
  * @tparam A the point component type parameter.
  * @tparam B the point weight type parameter.
  */
case class Sequence[A:Ordering, B:Monoid : Ordering](points: Seq[Point[A,B]])  {

  val weight: B = {
    val m = implicitly[Monoid[B]]
    points.map(_.w).fold(m.zero)(m.op)
  }

  def dominates(that: Sequence[A,B]): Boolean ={
    val thisLast = this.points.last
    val thatLast = that.points.last

    val oA = implicitly[Ordering[A]]
    val oB = implicitly[Ordering[B]]
    (oA.gteq(thatLast.y,thisLast.y) && oB.gt(thisLast.w,thatLast.w)) ||
      (oA.gt(thatLast.y,thisLast.y) && oB.gteq(thisLast.w,thatLast.w))
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
case class Point[A, B](x: A, y: A, w: B)(implicit val oA:Ordering[A], val oB:Ordering[B])

object Point {
  implicit def ordering[A:Ordering,B:Ordering]:Ordering[Point[A,B]] = implicitly[Ordering[(A,A,B)]].on(p => (p.x, p.y, p.w))
}

/**
  * A set of points.
  * @param points the points composing the set.
  * @param ordering the ordering for points.
  * @param orderingA the ordering for point components.
  * @tparam A the point type parameter.
  */
case class PointSet[A,B](points: TreeSet[Point[A,B]])(implicit ordering:Ordering[Point[A,B]], orderingA:Ordering[A]){

  type Pnt = Point[A,B]
  /**
    * Returns the point having the greatest y component among those strictly lower than that of the current point, if any
    * @param curr the current point
    * @return the point having the greatest y component among those strictly lower than that of the current point, if any
    */
  def predecessor(curr: Pnt): Option[Pnt] = {
    util.Try(points.filter(p => orderingA.lt(p.x, curr.x) && orderingA.lt(p.y, curr.y)).maxBy(_.y)).toOption
  }

  /**
    * Returns the point having the lowest y component among those strictly greater than that of the current point, if any
    * @param curr the current point
    * @return the point having the lowest y component among those strictly greater than that of the current point, if any
    */
  def successor(curr: Pnt): Option[Pnt] = {
    util.Try(
      if(curr == null)// || orderingA.lt(curr.x, 0))
        points.head
      else
        points.filterNot(_ equals curr)
        .keysIteratorFrom(curr)
        .min(points.ordering)
      ).toOption//.orElse(points.headOption)
  }

  /**
    * Removes the given element from the set.
    * @param p the element to remove.
    * @return true if the element was present in set, false otherwise.
    */
  def remove(p: Pnt): Boolean = points.remove(p)

  /**
    * Adds the given element to the set.
    * @param p the element to add.
    * @return true if the element was not yet in the set, false otherwise.
    */
  def add(p: Pnt): Boolean = points.add(p)

  /**
    * Returns the maximal element in the set.
    * @return the maximal element in the set.
    */
  def max(): Pnt = points.max

  /**
    * Returns the size of the set.
    * @return the size of the set.
    */
  def size(): Int = points.size

  override def toString(): String = points.mkString(", ")
}