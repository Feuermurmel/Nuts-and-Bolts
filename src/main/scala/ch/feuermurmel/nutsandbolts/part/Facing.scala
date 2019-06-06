package ch.feuermurmel.nutsandbolts.part

import ch.feuermurmel.nutsandbolts.body.Surface
import ch.feuermurmel.nutsandbolts.body.Surface.cone

/**
  * Operation applied to the faces of a surface (planes perpendicular to the z-axes by which a sliced surface is limited).
  */
case class Facing(fn: Surface => Surface) {
  def apply(surface: Surface) = fn(surface)
}

object Facing {
  def none = Facing(identity)

  def outsideChamfer(r: Double) = Facing(_ & cone(0, r, 1))
  def insideChamfer(r: Double) = Facing(_ | cone(0, r, -1))
}
