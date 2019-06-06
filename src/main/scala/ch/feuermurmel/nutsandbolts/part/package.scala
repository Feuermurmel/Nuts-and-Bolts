package ch.feuermurmel.nutsandbolts

import ch.feuermurmel.nutsandbolts.body.Surface

package object part {
//  def simpleBolt(thread: part.Thread, head: BoltHead, threadLength: Double) =
//    Body.withoutHole(
//      sliceWithFacings(
//        head.surface,
//        head.nominalHeight,
//        head.cutoffFacing,
//        head.cutoffFacing),
//      sliceWithFacings(
//        thread.outwardSurface,
//        threadLength,
//        thread.maleGraftFacing,
//        thread.maleCutoffFacing))

//  def threadedRod(thread: part.Thread, threadLength: Double) =
//    Body.withoutHole(
//      sliceWithFacings(
//        thread.outwardSurface,
//        threadLength,
//        thread.maleCutoffFacing,
//        thread.maleCutoffFacing))

//  def nut(thread: Thread, head: BoltHead) =
//    CylindricalBody(
//      sliceWithFacings(
//        head.surface,
//        head.nominalHeight,
//        head.cutoffFacing,
//        head.cutoffFacing)
//      / sliceWithFacings(
//        thread.inwardSurface,
//        head.nominalHeight,
//        thread.femaleCutoffFacing,
//        thread.femaleCutoffFacing),
//      0,
//      head.nominalHeight)

  private def sliceWithFacings(surface: Surface, height: Double, bottomFacing: Facing, topFacing: Facing) = {
    def flipPart(surface: Surface) = Surface(p => surface(height - p.z, p.c))

    flipPart(topFacing(flipPart(bottomFacing(surface))))
  }
}
