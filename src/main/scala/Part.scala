import ch.feuermurmel.nutsandbolts.body.Body
import ch.feuermurmel.nutsandbolts.util.Arguments

case class Part(name: String, argumentNames: Seq[String], createBody: Arguments => Body)

object Part {
  def apply(name: String, argumentNamesStr: String)(createBody: Arguments => Body): Part =
    Part(name, argumentNamesStr.split(" "), createBody)

  def run(parts: Seq[Part], args: Seq[String]) =
    args match {
      case Seq() =>
        throw new Exception("No part name specified.")
      case Seq(partName, args @ _*) =>
        val part = parts.find(_.name == partName).getOrElse(
          throw new Exception(s"Unknown part: $partName"))

        val arguments = Arguments(part.argumentNames, args)

        val argumentNameParts = part.argumentNames
          .flatMap(x => arguments.getStringList(x).map(x + _))

        (part.createBody(arguments), (partName +: argumentNameParts).mkString("-"))
    }
}
