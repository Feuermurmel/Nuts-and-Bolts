package ch.feuermurmel.nutsandbolts.util

object AutoCloseableUtil {
  def withCloseable[A <: AutoCloseable, R](getResource: => A)(block: A => R) = {
    val resource = getResource

    try {
      block(resource)
    } finally {
      resource.close()
    }
  }
}
