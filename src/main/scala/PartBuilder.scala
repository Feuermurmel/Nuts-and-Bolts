case class PartBuilder(slices: Seq[Part.Slice] = Seq()) {
  def up(surface: Surface, height: Double) =
    PartBuilder(slices :+ Part.Slice(surface, height, reversed = false))

  def down(surface: Surface, height: Double) =
    PartBuilder(slices :+ Part.Slice(surface, height, reversed = true))

  def buildWithoutHole = Part(slices, opened = false)

  def buildWithHole = Part(slices, opened = true)
}
