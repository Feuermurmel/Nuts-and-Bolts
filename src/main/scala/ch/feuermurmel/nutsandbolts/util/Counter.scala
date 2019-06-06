package ch.feuermurmel.nutsandbolts.util

import scala.collection.mutable

class Counter[A] {
  private val countsByElement = mutable.Map[A, Int]()

  private def update(element: A, countDiff: Int): Unit = {
    val newCount = this(element) + countDiff

    if (newCount == 0)
      countsByElement.remove(element)
    else
      countsByElement.update(element, newCount)
  }

  def apply(element: A) = countsByElement.getOrElse(element, 0)

  def entries = countsByElement.toSeq

  def keys = countsByElement.keys.toSeq

  def +=(element: A) = update(element, 1)

  def -=(element: A) = update(element, -1)
}
