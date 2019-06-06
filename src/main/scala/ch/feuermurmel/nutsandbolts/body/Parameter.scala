package ch.feuermurmel.nutsandbolts.body

/**
  * Represents a point in the coordinate system over which surfaces are parameterized.
  *
  * @param z The offset along the z-axis.
  * @param c The rotation around the z-axis.
  */
case class Parameter(z: Double, c: Double)
