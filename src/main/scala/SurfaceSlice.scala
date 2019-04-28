
case class SurfaceSlice(surface: Surface, height: Double, orientation: SurfaceSlice.Orientation) {
  def invert = copy(
    orientation = orientation match {
      case SurfaceSlice.Orientation.Outward => SurfaceSlice.Orientation.Inward
      case SurfaceSlice.Orientation.Inward => SurfaceSlice.Orientation.Outward
    })
}

object SurfaceSlice {
  sealed trait Orientation

  object Orientation {
    object Outward extends Orientation
    object Inward extends Orientation
  }
}
