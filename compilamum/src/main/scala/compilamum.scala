package compilamum

import parser.{Parseamum,ParseError}
import netCall.{GenNetCall, NetCallError}
import flatten.{FlattenNetCall,FlattenError}
import generateFunctions.{GenFunc,GenerateError}
import translate.{Translate,TranslateError}

object Main {
  def main(args: Array[String]) = {
    val code = "\"Hello world\";"
    Parseamum(code) flatmap GenNetCall flatmap FlattenNetCall flatmap GenFunc flatmap Translate match{
      case ParseError(_,_,_) => ???
      case NetCallError() => ???
      case FlattenError() => ???
      case GenerateError() => ???
      case TranslateError() => ???
      case x => ???
    }
  }
}

abstract class ErrorMum
