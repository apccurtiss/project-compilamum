package compilamum

import parser.{Parse,ParseError}
import netCall.{GenNetCall, NetCallError}
import flatten.{FlattenNetCall,FlattenError}
import cutter.{Cut,CutError}
import generater.{Generate,GenerateError}
import runtime.{AddFrontendRuntime,AddBackendRuntime}

import java.io._

object Main {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile(args(0))
    val code = try source.mkString finally source.close()
    Compile(code) match {
      case Right((client, server)) => {
        val root = args(0).substring(0, args(0).lastIndexOf("."))
        save(client, s"${root}-client.js")
        save(server, s"${root}-server.js")
      }
      case Left(ParseError(row, col, msg)) => {
        printErr(s"$code ".split("\n")(row), col, s"${row+1}:${col+1} Parse error: $msg")
      }
      case Left(NetCallError()) => println("Net call error.")
      case Left(FlattenError()) => println("Flatten error.")
      case Left(CutError()) => println("Cut error.")
      case Left(GenerateError(msg)) => println(s"Generate error: $msg")
      case _ => ???
    }
  }

  def printErr(line: String, col: Int, msg: String): Unit = {
    println(msg)
    println(line)
    println(s"${" " * col}^")
  }

  def save(code: String, path: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(new File(path)))
    bw.write(code)
    bw.close()
  }
}

object Compile {
  def apply(code: String): Either[ErrorMum, (String, String)] = {
    val ast = Parse(code) flatMap GenNetCall.apply flatMap FlattenNetCall.apply flatMap Cut.apply
    ast flatMap {
      case (client, server) => Generate(client) flatMap {
        client => Generate(server) map {
          server => (AddFrontendRuntime(client), AddBackendRuntime(server))
        }
      }
    }
  }
}

abstract class ErrorMum extends Throwable
