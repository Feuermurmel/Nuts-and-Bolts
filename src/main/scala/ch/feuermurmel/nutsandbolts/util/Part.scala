package ch.feuermurmel.nutsandbolts.util

import ch.feuermurmel.nutsandbolts.body.Body

case class Part(body: Body, resolution: Double, fileBaseName: String)

object Part {
  case class Def(name: String, argumentDef: Arguments.Def, createBody: Arguments => Body)

  def define(name: String, argumentNamesStr: String)(createBody: Arguments => Body): Def =
    Def(name, Arguments.Def(argumentNamesStr.split(" ") :+ "res"), createBody)

  def run(parts: Seq[Def], args: Seq[String]) =
    {
      def usageHint = s"Available parts are ${parts.map(_.name).mkString(", ")}."

      args match {
        case Seq() =>
          throw new UserError(s"No part name specified. $usageHint")
        case Seq(partName, args @ _*) =>
          val part = parts.find(_.name == partName).getOrElse(
            throw new UserError(s"Unknown part $partName specified. $usageHint"))

          UserError.augmenting(m => s"${part.name}: $m") {
            part.argumentDef.runWithArgs(args) { arguments =>
              val argumentNameParts = part.argumentDef.argumentNames
                .flatMap(x => arguments.getStringList(x).map(x + _))

              Part(
                body = part.createBody(arguments),
                resolution = arguments.get("res", 0.1),
                fileBaseName = (partName +: argumentNameParts).mkString("-"))
            }
          }
      }
    }
}
