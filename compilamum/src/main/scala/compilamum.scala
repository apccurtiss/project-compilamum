import parser.{Parseamum,ParseError}

object Main {
  def main(args: Array[String]) = {
    val code = "while (1 + 1) { 1 +2; }"
    Parseamum(code) match {
      case Left(ParseError(line, column, msg)) => {
        println("Error: " + msg)
        println(code.split("\n")(line))
        println(" " * column + "^")
      }
      case Right(ast) => println("AST: " + ast)
    }
  }
}