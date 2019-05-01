package ch.feuermurmel.nutsandbolts.polyhedron

case class Face(p1: Point, p2: Point, p3: Point) {
  // TODO: Maybe we should handle polygons with zero area.
  def normal = ((p2 - p1) cross (p3 - p1)).normalized
}
