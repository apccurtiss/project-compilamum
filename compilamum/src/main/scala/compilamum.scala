package compilamum

import parser.{Parse,ParseError}
import netCall.{GenNetCall, NetCallError}
import flatten.{FlattenNetCall,FlattenError}
import cutter.{Cut,CutError}
import generater.{Generate,GenerateError}

object Main {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile(args(0))
    val code = try source.mkString finally source.close()
    Compile(code)
  }
}

object Compile {
  def apply(code: String): Unit = {
    Parse(code) flatMap GenNetCall.apply flatMap FlattenNetCall.apply flatMap Cut.apply flatMap Generate.apply match {
      case x => println(x)
    }
  }
}

abstract class ErrorMum
