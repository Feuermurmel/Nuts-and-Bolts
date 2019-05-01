package ch.feuermurmel.nutsandbolts.polyhedron

case class Point(x: Double, y: Double, z: Double) {
  def +(other: Point) = Point(x + other.x, y + other.y, z + other.z)
  def *(other: Double) = Point(x * other, y * other, z * other)
  def /(other: Double) = this * (1 / other)
  def -(other: Point) = this + -other
  def unary_- = this * -1

  def dot(other: Point) = x * other.x + y * other.y + z * other.z
  def normSq = this dot this
  def norm = Math.sqrt(normSq)
  def normalized = this / norm

  def cross(other: Point) =
    Point(
      y * other.z - z * other.y,
      z * other.x - x * other.z,
      x * other.y - y * other.x)
}
