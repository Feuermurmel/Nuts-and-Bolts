package ch.feuermurmel.nutsandbolts.body

import ch.feuermurmel.nutsandbolts.util.SeqBuilder

case class Ray(intervals: Seq[Interval]) {
  if (intervals.nonEmpty) {
    // Check that intervals are in ascending order and do not overlap.
    intervals.zip(intervals.tail).foreach({ case (a, b) =>
      require(a.end < b.start)
    })
  }

  def shift(offset: Double) = {
    val builder = new Ray.Builder()

    intervals.foreach({ interval =>
      builder.addInterval(interval.start + offset, interval.end + offset)
    })

    builder.build
  }

  def hull =
    if (intervals.isEmpty)
      None
    else
      Some(Interval(intervals.head.start, intervals.last.end))

  def isEmpty = intervals.isEmpty

  def nonEmpty = !isEmpty

  def overlaps(other: Ray) = (this & other).nonEmpty

  def contains(other: Ray) = (other / other).isEmpty

  def unary_! = {
    val starts = Double.NegativeInfinity +: intervals.map(_.end)
    val ends = intervals.map(_.start) :+ Double.PositiveInfinity

    Ray(starts.zip(ends).collect({ case (s, e) if s != e => Interval(s, e) }))
  }

  def &(other: Ray) = {
    var a = intervals
    var b = other.intervals
    var res = Seq[Interval]()

    while (a.nonEmpty && b.nonEmpty) {
      (a.head & b.head).foreach(res :+= _)

      if (a.head.end < b.head.end)
        a = a.tail
      else
        b = b.tail
    }

    Ray(res)
  }

  def |(other: Ray) = !(!this & !other)

  def /(other: Ray) = this & !other
}

object Ray {
  val empty = Ray(Seq())

  def apply(start: Double, end: Double): Ray =
    Ray(Seq(Interval(start, end)))

  private class Builder {
    private val intervals = new SeqBuilder[Interval]

    def addInterval(start: Double, end: Double) =
      if (start != end) {
        if (!intervals.isEmpty && intervals.last.end == start)
          intervals.last = Interval(intervals.last.start, end)
        else
          intervals += Interval(start, end)
      }

    def build = Ray(intervals.build)
  }

  implicit class IntervalAsRay(value: Interval) extends Ray(Seq(value))
}
