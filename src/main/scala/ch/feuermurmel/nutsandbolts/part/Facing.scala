package ch.feuermurmel.nutsandbolts.part

import ch.feuermurmel.nutsandbolts.surface.Surface
import ch.feuermurmel.nutsandbolts.surface.Surface.cone

/**
  * Operation applied to the faces of a surface (planes perpendicular to the z-axes by which a sliced surface is limited).
  */
trait Facing {
  def apply(surface: Surface): Surface
}

object Facing {
  def none: Facing = identity(_)

  def outsideChamfer(r: Double): Facing = _ & cone(0, r, 1)
  def insideChamfer(r: Double): Facing = _ | cone(0, r, -1)
}
