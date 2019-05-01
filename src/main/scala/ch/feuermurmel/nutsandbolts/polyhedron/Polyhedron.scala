package ch.feuermurmel.nutsandbolts.polyhedron

import java.nio.file.Path
import java.nio.{ByteBuffer, ByteOrder}

import ch.feuermurmel.nutsandbolts.util.PathUtil

case class Polyhedron(faces: Seq[Face]) {
  def writeToSTLFile(path: Path): Unit =
    PathUtil.writeUsingChannel(path) { channel =>
      val buffer = ByteBuffer.allocate(80 + 4)
      buffer.order(ByteOrder.LITTLE_ENDIAN)

      import buffer.{putFloat, putInt, putShort}

      def flush(): Unit = {
        buffer.flip()
        channel.write(buffer)
        buffer.clear()
      }

      def skip(count: Int) =
        buffer.position(buffer.position() + count)

      def putPoint(point: Point) = {
        putFloat(point.x.toFloat)
        putFloat(point.y.toFloat)
        putFloat(point.z.toFloat)
      }

      // Skip unused header part.
      skip(80)

      // Face count.
      putInt(faces.size)

      flush()

      faces.foreach({ face =>
        putPoint(face.normal)
        putPoint(face.p1)
        putPoint(face.p2)
        putPoint(face.p3)

        // Attribute size.
        putShort(0)

        flush()
      })
    }
}
