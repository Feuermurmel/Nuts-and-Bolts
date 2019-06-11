package ch.feuermurmel.nutsandbolts.util

case class Arguments(argumentNames: Seq[String], args: Seq[String]) {
  private val valuesStrByName = {
    require(argumentNames.distinct == argumentNames)

    val argumentPattern = "([a-zA-Z]+)(.*)".r

    args
      .map({ case argumentPattern(name, valueStr) =>
        if (!argumentNames.contains(name))
          throw new Exception(s"Unknown argument: $name")

        name -> valueStr
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  private def readList[A](name: String, decode: String => A) = {
    require(argumentNames.contains(name), s"$name")

    valuesStrByName.getOrElse(name, Seq()).map(decode)
  }

  private def readOption[A](name: String, decode: String => A) =
    readList(name, decode) match {
      case Seq() =>
        None
      case Seq(value) =>
        Some(value)
      case _ =>
        throw new Exception(s"Parameter specified multiple times: $name")
    }

  private def read[A](name: String, decode: String => A, default: => A): A =
    readOption(name, decode).getOrElse(default)

  private def read[A](name: String, decode: String => A): A =
    read(name, decode, throw new Exception(s"Missing parameter: $name"))

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
        throw new Exception(s"No value expected for parameter $name.")
      case None =>
        false
    }
}
