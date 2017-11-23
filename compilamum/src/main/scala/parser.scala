package parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

sealed trait AST extends Positional

case class Literal(n: Float) extends AST
case class Ident(x: String) extends AST
case class BinOp(l: AST, op: String, r: AST) extends AST

object Parseamum extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[String, List[AST]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(msg)
      case Success(result, next) => Right(result)
    }
  }

  def number: Parser[AST] = "[0-9]+".r ^^ { n => Literal(n.toFloat) }
  def ident: Parser[AST] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { x => Ident(x) }
  def atom: Parser[AST] = number | ident | "(" ~> expr <~ ")"
  def md: Parser[AST] = atom ~ rep("*" ~ atom | "/" ~ atom) ^^ {
    case l ~ list => (l /: list) {
      case (Literal(l), "*" ~ Literal(r)) => Literal(l * r)
      case (Literal(l), "/" ~ Literal(r)) => Literal(l / r)
      case (acc, op ~ next) => BinOp(acc, op, next)
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
  def tokens: Parser[List[AST]] = phrase(rep(expr <~ ";"))
}
