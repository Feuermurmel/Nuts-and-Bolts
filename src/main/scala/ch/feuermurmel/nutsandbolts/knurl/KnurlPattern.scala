package ch.feuermurmel.nutsandbolts.knurl

import java.lang.Math.{sin, tan}

import ch.feuermurmel.nutsandbolts.util.MathUtil.tau

import scala.language.reflectiveCalls

/**
  * @param advance  How much one spiral of the knurl advances over one revolution around the Z-axis.
  * @param distance Distance between a ridge and the next turn of itself perpendicular to the ridge.
  * @param ridges   Number of ridges placed parallel to each other.
  */
case class KnurlPattern(advance: Double, distance: Double, ridges: Int)

object KnurlPattern {
  def forApproximateStepSize(angle: Double, workpieceRadius: Double, approximateStepSize: Double) =
    withAngle(angle, workpieceRadius).forApproximateStepSize(approximateStepSize)

  def forApproximateAdvance(angle: Double, workpieceRadius: Double, approximateAdvance: Double) =
    withAngle(angle: Double, workpieceRadius: Double).forApproximateAdvance(approximateAdvance: Double)

  def forRidgeCount(angle: Double, workpieceRadius: Double, ridges: Int) =
    withAngle(angle, workpieceRadius).forRidgeCount(ridges)

  private def withAngle(angle: Double, workpieceRadius: Double) = new {
    val circumference = tau * workpieceRadius
    val distance = circumference * sin(angle)
    val advance = circumference * tan(angle)

    def forApproximateStepSize(approximateStepSize: Double) =
      forRidgeCount(Math.ceil(distance / approximateStepSize).toInt)

    def forApproximateAdvance(approximateAdvance: Double) =
      forRidgeCount(Math.ceil(advance / approximateAdvance).toInt)

    def forRidgeCount(ridges: Int) =
      KnurlPattern(advance, distance, ridges)
  }
}
