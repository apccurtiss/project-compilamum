package compilamum

import parser.{Parse,ParseError}
import typechecker.{Typecheck,TypeError}
import cutter.{Cut,CutError}
import generator.{Generate,GenerateError}

import java.io._

abstract class Erramum extends Throwable

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
      case Left(TypeError()) => println("Type error.")
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
  def apply(code: String): Either[Erramum, (String, String)] = {
    for {
      ast <- Parse(code)
      checked_ast <- Typecheck(ast)
      pair <- Cut(checked_ast)
      (client, server) = pair
      client_out <- Generate(client)
      server_out <- Generate(server)
    } yield (client_out, server_out)
  }
}
