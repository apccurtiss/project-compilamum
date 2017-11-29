import parser.{Parseamum,ParseError}
import netCall.{genNetCall, NetCallError}
import flatten.{flattenNetCall,FlattenError}
import generateFunctions.{genFunc,GenerateError}
import translate.{translate,TranslateError}

object Main {
  def main(args: Array[String]) = {
    val code = "\"Hello world\";"
    Parseamum(code) flatmap genNetCall flatmap flattenNetCall flatmap genFunc flatmap translate match{
      case ParseError(_,_,_) => ???
      case NetCallError(_) => ???
      case FlattenError(_) => ???
      case GenerateError(_) => ???
      case TranslateError(_) => ???
      case x => ???
    }
  }
}

abstract class ErrorMum
