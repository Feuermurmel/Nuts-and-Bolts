package ch.feuermurmel.nutsandbolts.util

import java.lang.Math.max

import scala.reflect.ClassTag

class SeqBuilder[A] {
  private var array: Array[A] = null
  private var count = 0

  private def ensureSize(size: Int)(implicit ct: ClassTag[A]) =
    if (array == null) {
      array = new Array(max(size, 4))
    } else if (size > array.length) {
      val oldArr = array

      array = new Array(max(size, array.length * 2))

      oldArr.copyToArray(array)
    }

  def +=(element: A)(implicit ct: ClassTag[A]) = {
    ensureSize(count + 1)

    array(count) = element
    count += 1
  }

  def ++=(elements: Seq[A])(implicit ct: ClassTag[A]) = {
    ensureSize(count + elements.size)

    elements.copyToArray(array, count)
    count += elements.size
  }

  def isEmpty = count == 0

  def last = array(count - 1)

  def last_=(newElement: A) = array(count - 1) = newElement

  def build =
    if (array == null)
      Seq()
    else
      array.take(count).toSeq
}
