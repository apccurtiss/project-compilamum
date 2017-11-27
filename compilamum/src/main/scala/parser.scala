package parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

import ast._

case class ParseError(line: Int, column: Int, msg: String)

// case class Literal(n: Float) extends AST
// case class Ident(x: String) extends AST
// case class BinOp(l: AST, op: String, r: AST) extends AST
// case class While(cond: AST, body: List[AST]) extends AST

object Parseamum extends RegexParsers {
  override def failure(msg: String) = "" ~> super.failure(msg)
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[ParseError, List[ast.Node]] = {
    parse(program, code) match {
      case NoSuccess(msg, next) => Left(ParseError(next.pos.line-1, next.pos.column-1, msg))

      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[List[ast.Node]] = phrase(rep(global)) | failure("Reached EOF")

  def global: Parser[ast.Node] = global_decl | import_stmt | function
  def global_decl: Parser[ast.Node] = location ~ typ ~ name ~ "=" ~> expr <~ ";"
  def import_stmt: Parser[ast.Node] = "import" ~> location ~ "as" ~> name ~ \
    ("(" ~> params <~ ")" <~ ";"

  def atom: Parser[ast.Node] = const | name | "(" ~> expr <~ ")" | failure("Unexpected end of line.")

  def bool: Parser[ast.Node] = "(True)|(False)".r ^^ { b => ast.ConstBool(n == "True") }
  def string: Parser[ast.Node] = "(:?[^\"]|\\\")*".r ^^ { s => ast.ConstString(s) }
  def number: Parser[ast.Node] = "\\d+(:?\\.\\d*)?|\\.\\d+".r ^^ { n => ast.ConstFloat(n.toFloat) }

  def name: Parser[ast.Node] = "[\\w_][\\w_\\d]*".r ^^ { x => ast.Name(x) }

  def md: Parser[AST] = atom ~ rep("*" ~ atom | "/" ~ atom) ^^ {
    case l ~ list => (l /: list) {
      case (Literal(l), "*" ~ Literal(r)) => Literal(l * r)
      case (Literal(l), "/" ~ Literal(r)) => Literal(l / r)
      case (acc, op ~ next) => Bop(acc, op, next)
    }
  }

  def as: Parser[AST] = md ~ rep("+" ~ md | "-" ~ md) ^^ {
    case l ~ list => (l /: list) {
      case (Literal(l), "+" ~ Literal(r)) => Literal(l + r)
      case (Literal(l), "-" ~ Literal(r)) => Literal(l - r)
      case (acc, op ~ next) => BinOp(acc, op, next)
    }
  }
  def expr: Parser[AST] = as
  def wloop: Parser[AST] =  ("while" ~> "(" ~> expr <~ ")") ~ ("{" ~> rep(stmt) <~ "}") ^^ {
    case cond ~ body => While(cond, body)
  }
  def stmt: Parser[AST] = expr <~ ";" | wloop
}
