import ch.feuermurmel.nutsandbolts.body.Surface.{cylinder, sphere}
import ch.feuermurmel.nutsandbolts.knurl.{Knurl, KnurlMode, KnurlPattern, KnurlShape}
import ch.feuermurmel.nutsandbolts.part.{BoltHead, Facing}
import ch.feuermurmel.nutsandbolts.util.MathUtil.tau

object Stuff {
  /**
    * @param size The size of a bolt head for the same size of bolt.
    */
  def knurledRingNut(size: Double) = {
    val radius = size / 2

    val shape = KnurlShape(0, 0.5, 1)

    val pattern = KnurlPattern.forRidgeCount(
      angle = tau / 12,
      workpieceRadius = radius,
      ridges = 15)

    val knurl = Knurl(
      pattern,
      shape,
      KnurlMode.BothSubtractive)

    new BoltHead {
      override def surface = knurl.applyToSurface(cylinder(radius))
      override def cutoffFacing = Facing.outsideChamfer(radius - knurl.depth)
      override def nominalHeight = size / 4
    }
  }

  /**
    * @param size The size of a bolt head for the same size of bolt.
    */
  def knurledRoundedRingNut(size: Double, height: Double) = {
    val radius = size / 2

    val shape = KnurlShape(0, 0.5, 1)

    val pattern = KnurlPattern.forApproximateAdvance(
      angle = tau / 12,
      workpieceRadius = radius,
      approximateAdvance = height / 3)

    val knurl = Knurl(
      pattern,
      shape,
      KnurlMode.BothSubtractive)

    val sphereRadius = height * 2 / 3

    val nutSurface = sphere(sphereRadius).grow(radius - sphereRadius)

    new BoltHead {
      // Shift knurled nut here so that the knurling is symmetric along the Z-axis.
      override def surface = knurl.applyToSurface(nutSurface).shift(height / 2)
      override def cutoffFacing = Facing.none
      override def nominalHeight = height
    }
  }
}
