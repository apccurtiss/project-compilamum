import parser.Parseamum

object Main {
  def main(args: Array[String]) = {
    Parseamum("1 * 2;") match {
      case Left(msg) => println("Error: " + msg)
      case Right(ast) => println("AST: " + ast)
    }
  }
}
