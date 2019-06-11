import ch.feuermurmel.nutsandbolts.body.Body
import ch.feuermurmel.nutsandbolts.util.Arguments

case class Part(body: Body, resolution: Double, fileBaseName: String)

object Part {
  case class Def(name: String, argumentNames: Seq[String], createBody: Arguments => Body)

  def define(name: String, argumentNamesStr: String)(createBody: Arguments => Body): Def =
    Def(name, argumentNamesStr.split(" ") :+ "res", createBody)

  def run(parts: Seq[Def], args: Seq[String]) =
    args match {
      case Seq() =>
        throw new Exception("No part name specified.")
      case Seq(partName, args @ _*) =>
        val part = parts.find(_.name == partName).getOrElse(
          throw new Exception(s"Unknown part: $partName"))

        val arguments = Arguments(part.argumentNames, args)

        val argumentNameParts = part.argumentNames
          .flatMap(x => arguments.getStringList(x).map(x + _))

        Part(
          body = part.createBody(arguments),
          resolution = arguments.get("res", 0.1),
          fileBaseName = (partName +: argumentNameParts).mkString("-"))
    }
}
