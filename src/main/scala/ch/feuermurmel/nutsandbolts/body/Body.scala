package ch.feuermurmel.nutsandbolts.body

import java.lang.Math.floorMod

import ch.feuermurmel.nutsandbolts.polyhedron.{Face, Point, Polyhedron}
import ch.feuermurmel.nutsandbolts.util.Counter

trait Body {
  def toPolyhedron(resolution: Double): Polyhedron
}

object Body {
  // 0.0 == -0.0 but not 0.0.equals(-0.0). Adding 0 fixes ordering of points by their coordinates.
  private implicit val pointOrdering: Ordering[Point] =
    Ordering.by(p => (p.x + 0, p.y + 0, p.z + 0))

  private implicit class OrderingOps[A: Ordering](value: A) {
    def sm(other: A) = Ordering[A].lt(value, other)
  }

  def normalizeEdge(p1: Point, p2: Point) =
    if (p1 sm p2)
      ((p1, p2), false)
    else
      ((p2, p1), true)

  def normalizeFace(p1: Point, p2: Point, p3: Point) = {
    var (q1, q2, q3) = (p1, p2, p3)

    if (p2 sm q1) {q1 = p2; q2 = p3; q3 = p1}
    if (p3 sm q1) {q1 = p3; q2 = p1; q3 = p2}

    if (q2 sm q3) ((q1, q2, q3), false) else ((q1, q3, q2), true)
  }

  // TODO: Complete this comment.
  /**
    * Construct a polyhedron from a function which evaluates to rays at grid points in a rectangular grid. The grid may be wrapped around in one or both directions.
    *
    * @param cellsI
    * @param cellsJ
    * @param combineIndex
    * @param rayAtIndex
    * @param pointOnRay
    * @param wrapI
    * @param wrapJ
    * @tparam A
    * @return
    */
  def constructPolyhedron[A](
    sizeI: Int,
    sizeJ: Int,
    combineIndex: (Int, Int) => A,
    rayAtIndex: A => Ray,
    pointOnRay: (A, Double) => Point,
    wrapI: Boolean = false,
    wrapJ: Boolean = false
  ) = {
    // This, and using either cells* or size* in the right places is the only magic necessary for supporting optional wrapping in both dimensions. Everything else just falls into place perfectly.
    val cellsI = sizeI - (if (wrapI) 0 else 1)
    val cellsJ = sizeJ - (if (wrapJ) 0 else 1)

    val indexes = (0 until sizeJ).map(j => (0 until sizeI).map(i => combineIndex(i, j)))

    val rays = indexes.map(_.map(rayAtIndex))

    case class Event(value: Double, point: Point)

    object Event {
      // It's very import for generating a consistent polyhedron that there's a total ordering on all points when constructing faces on a side but that this ordering is also consistent with the partial ordering given by the values on the corresponding rays from which the points were generated.
      implicit val ordering: Ordering[Event] = Ordering.by(x => (x.value, x.point))
    }

    object Faces {
      var faceCounter = new Counter[(Point, Point, Point)]

      def add(p1: Point, p2: Point, p3: Point): Unit = {
        val ((q1, q2, q3), parity) = normalizeFace(p1, p2, p3)

        // Ignore faces with zero area.
        if (q1 != q2 && q2 != q3) {
          if (parity)
            faceCounter -= (q1, q2, q3)
          else
            faceCounter += (q1, q2, q3)
        }
      }

      def addStrip(events1: Seq[Event], events2: Seq[Event]): Unit = {
        var head1 = events1.head
        var tail1 = events1.tail

        var head2 = events2.head
        var tail2 = events2.tail

        while (tail1.nonEmpty || tail2.nonEmpty) {
          if (tail2.isEmpty || tail1.nonEmpty && (head1 sm head2)) {
            val newHead = tail1.head

            Faces.add(head2.point, head1.point, newHead.point)

            head1 = newHead
            tail1 = tail1.tail
          } else {
            val newHead = tail2.head

            Faces.add(head2.point, head1.point, newHead.point)

            head2 = newHead
            tail2 = tail2.tail
          }
        }
      }

      def faces =
        faceCounter.entries.flatMap({ case ((p1, p2, p3), count) =>
          if (count > 0)
            Seq.fill(count)(Face(p1, p2, p3))
          else
            Seq.fill(-count)(Face(p1, p3, p2))
        })
    }

    def handleCell(a1: A, r1: Ray, a2: A, r2: Ray, a3: A, r3: Ray) = {
      // Spans are continuous regions of a cell where every points in the region is covered by at least one of the three rays and which contains sub-regions covered by each of the three rays.
      val spans = (r1 | r2 | r3).intervals.filter(x => Seq(r1, r2, r3).forall(_ overlaps x))

      spans.foreach({ span =>
        def events(index: A, ray: Ray) =
          (ray & span).intervals
            .flatMap(x => Seq(x.start, x.end))
            .map(x => Event(x, pointOnRay(index, x)))

        val events1 = events(a1, r1)
        val events2 = events(a2, r2)
        val events3 = events(a3, r3)

        Faces.addStrip(events1, events2)
        Faces.addStrip(events2, events3)
        Faces.addStrip(events3, events1)

        Faces.add(events1.head.point, events2.head.point, events3.head.point)
        Faces.add(events1.last.point, events3.last.point, events2.last.point)
      })
    }

    def handleIndex(i: Int, j: Int) = {
      val i2 = floorMod(i + 1, sizeI)
      val j2 = floorMod(j + 1, sizeJ)
      def get(i: Int, j: Int) = (indexes(j)(i), rays(j)(i))

      val (a11, r11) = get(i, j)
      val (a12, r12) = get(i, j2)
      val (a21, r21) = get(i2, j)
      val (a22, r22) = get(i2, j2)

      handleCell(a11, r11, a12, r12, a22, r22)
      handleCell(a11, r11, a22, r22, a21, r21)
    }

    (0 until cellsJ).foreach(j => (0 until cellsI).foreach(i => handleIndex(i, j)))

    Polyhedron(Faces.faces)
  }
}
