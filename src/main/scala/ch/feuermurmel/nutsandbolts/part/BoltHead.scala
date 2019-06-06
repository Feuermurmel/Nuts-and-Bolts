package ch.feuermurmel.nutsandbolts.part

import ch.feuermurmel.nutsandbolts.body.Surface

trait BoltHead {
  def surface: Surface
  def cutoffFacing: Facing
  def nominalHeight: Double
}
