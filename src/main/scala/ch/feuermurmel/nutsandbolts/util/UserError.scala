package ch.feuermurmel.nutsandbolts.util

class UserError(message: String) extends Exception(message)

object UserError {
  def catching[R](block: => R) =
    try {
      block
    } catch {
      case e: UserError => println(s"error: ${e.getMessage}")
    }

  def augmenting[R](augmentMessage: String => String)(block: => R) =
    try {
      block
    } catch {
      case e: UserError => throw new UserError(augmentMessage(e.getMessage))
    }
}
