package ch.feuermurmel.nutsandbolts.util

import java.nio.channels.WritableByteChannel
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}

import ch.feuermurmel.nutsandbolts.util.AutoCloseableUtil.withCloseable

object PathUtil {
  def deleteDirectory(path: Path): Unit = {
    if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS))
      Files.list(path).forEach(deleteDirectory)

    Files.delete(path)
  }

  def usingTemporaryDirectory[A](block: Path => A) = {
    val directory = Files.createTempDirectory(null)

    try {
      block(directory)
    } finally {
      deleteDirectory(directory)
    }
  }

  def usingTemporaryFile(path: Path)(block: Path => Unit): Unit = {
    val parentDir = path.toAbsolutePath.getParent
    val tempPath = parentDir.resolve(path.getFileName.toString + "~")

    Files.createDirectories(parentDir)

    block(tempPath)

    Files.move(tempPath, path, StandardCopyOption.REPLACE_EXISTING)
  }

  def writeUsingChannel(path: Path)(block: WritableByteChannel => Unit) =
    usingTemporaryFile(path) { tempPath =>
      withCloseable(Files.newByteChannel(tempPath, StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) { channel =>
        block(channel)
      }
    }
}
