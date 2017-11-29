package compilamum

import parser.{Parseamum,ParseError}
import netCall.{GenNetCall, NetCallError}
import flatten.{FlattenNetCall,FlattenError}
import generateFunctions.{GenFunc,GenerateError}
import translate.{Translate,TranslateError}

object Main {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile(args(0))
    val code = try source.mkString finally source.close()
    Parseamum(code) flatMap GenNetCall.apply flatMap FlattenNetCall.apply flatMap GenFunc.apply flatMap Translate.apply match {
      case x => println(x)
    }
  }
}

abstract class ErrorMum
