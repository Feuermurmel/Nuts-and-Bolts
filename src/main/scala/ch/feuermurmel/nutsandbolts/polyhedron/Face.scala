package ch.feuermurmel.nutsandbolts.polyhedron

case class Face(p1: Point, p2: Point, p3: Point) {
  // TODO: Maybe we should handle polygons with zero area.
  def normal = {
    val normal = ((p2 - p1) cross (p3 - p1)).normalized

    if (normal.x.isNaN || normal.y.isNaN || normal.z.isNaN)
      Point(0, 0, 0)
    else
      normal
  }
}
