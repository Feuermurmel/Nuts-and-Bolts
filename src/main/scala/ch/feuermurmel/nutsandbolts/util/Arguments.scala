package ch.feuermurmel.nutsandbolts.util

case class Arguments(argumentDef: Arguments.Def, args: Seq[String]) {
  private val valuesStrByName = {
    val argumentPattern = "([a-zA-Z]+)(.*)".r

    args
      .map({ case argumentPattern(name, valueStr) =>
        if (!argumentDef.argumentNames.contains(name))
          throw new Arguments.UsageError(s"Unknown argument $name specified.")

        name -> valueStr
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  private def readList[A](name: String, decode: String => A) = {
    require(argumentDef.argumentNames.contains(name), s"$name")

    valuesStrByName.getOrElse(name, Seq()).map(decode)
  }

  private def readOption[A](name: String, decode: String => A) =
    readList(name, decode) match {
      case Seq() =>
        None
      case Seq(value) =>
        Some(value)
      case _ =>
        throw new Arguments.UsageError(s"Parameter $name specified multiple times.")
    }

  private def read[A](name: String, decode: String => A, default: => A): A =
    readOption(name, decode).getOrElse(default)

  private def read[A](name: String, decode: String => A): A =
    read(name, decode, throw new Arguments.UsageError(s"Missing parameter $name."))

  def getStringList(name: String) =
    readList(name, identity)

  def getList(name: String) =
    readList(name, _.toDouble)

  def getOption(name: String) =
    readOption(name, _.toDouble)

  def get(name: String, default: => Double): Double =
    read(name, _.toDouble, default)

  def get(name: String): Double =
    read(name, _.toDouble)

  def getBoolean(name: String) =
    readOption(name, identity) match {
      case Some("") =>
        true
      case Some(_) =>
        throw new Arguments.UsageError(s"No value expected for parameter $name.")
      case None =>
        false
    }
}

object Arguments {
  case class Def(argumentNames: Seq[String]) {
    require(argumentNames.distinct == argumentNames)

    def runWithArgs[R](args: Seq[String])(block: Arguments => R) =
      try {
        block(Arguments(this, args))
      } catch {
        case e: UsageError =>
          throw new UserError(s"${e.getMessage} Available arguments are ${argumentNames.mkString(", ")}.")
      }
  }

  class UsageError(message: String) extends Exception(message)
}
