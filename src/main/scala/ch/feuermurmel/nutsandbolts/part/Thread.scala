package ch.feuermurmel.nutsandbolts.part

import ch.feuermurmel.nutsandbolts.surface.Surface

trait Thread {
  def outwardSurface: Surface
  def inwardSurface: Surface

  def maleCutoffFacing: Facing
  def maleGraftFacing: Facing

  def femaleCutoffFacing: Facing
  def femaleGraftFacing: Facing
}
