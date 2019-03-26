package util

object SeqUtil {
  def mapSlidingPairs[A, B](seq: Seq[A])(fn: (A, A) => B) =
    seq.sliding(2).map({ case Seq(a, b) => fn(a, b) }).toSeq

  def mapCyclicSlidingPairs[A, B](seq: Seq[A])(fn: (A, A) => B) =
    mapSlidingPairs(seq :+ seq.head)(fn)
}
