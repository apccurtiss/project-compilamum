package compilamum

import parser.{Parseamum,ParseError}
import netCall.{GenNetCall, NetCallError}
import flatten.{FlattenNetCall,FlattenError}
import generateFunctions.{GenFunc,GenerateError}
import translate.{Translate,TranslateError}

object Main {
  def main(args: Array[String]) = {
    val code = "\"Hello world\";"
    Parseamum(code) flatMap GenNetCall.apply flatMap FlattenNetCall.apply flatMap GenFunc.apply flatMap Translate.apply map {
      case x => println(x)
    }
  }
}

abstract class ErrorMum
