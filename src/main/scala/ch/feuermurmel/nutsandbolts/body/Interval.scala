package ch.feuermurmel.nutsandbolts.body

import java.lang.Math.{max, min}

case class Interval(start: Double, end: Double) {
  require(!start.isNaN)
  require(!end.isNaN)
  require(start < end)

  def &(other: Interval) = {
    val newStart = max(start, other.start)
    val newEnd = min(end, other.end)

    if (newStart < newEnd)
      Some(Interval(newStart, newEnd))
    else
      None
  }
}
