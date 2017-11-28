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

  def apply(code: String): Either[ParseError, List[Node]] = {
    parse(program, code) match {
      case NoSuccess(msg, next) => Left(ParseError(next.pos.line-1, next.pos.column-1, msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[List[Node]] = phrase(rep(stmt)) | failure("Reached EOF")

  // THESE ARE COMMENTED OUT BECAUSE THEY ARE CORRECT BUT DO NOT COMPILE
  //def global: Parser[Global] = global_decl | import_stmt | function
  //def global_decl: Parser[Global] = location ~ typ ~ name ~ "=" ~> expr <~ ";"
  //def import_stmt: Parser[Global] = "import" ~> location ~ "as" ~> name ~ ("(" ~> params <~ ")") <~ ";"

  def atom: Parser[Expr] = const | name | "(" ~> expr <~ ")" | failure("Unexpected end of line.")

  def const: Parser[Expr] = bool | string | number
  def bool: Parser[Literal] = "(True)|(False)".r ^^ { b => ConstBool(b == "True") }
  def string: Parser[Literal] = ("\"" ~> "[^\"]*".r <~ "\"") ^^ { s => ConstString(s) }
  def number: Parser[Literal] = "\\d+(:?\\.\\d*)?|\\.\\d+".r ^^ { n => ConstFloat(n.toDouble) }

  def name: Parser[Literal] = not("break" | "continue") ~> "[\\w_][\\w_\\d]*".r ^^ { x => Name(x) }

  def md: Parser[Expr] = atom ~ rep("*" ~ atom | "/" ~ atom) ^^ {
    case l ~ list => (l /: list) {
      case (ConstFloat(l), "*" ~ ConstFloat(r)) => ConstFloat(l * r)
      case (ConstFloat(l), "/" ~ ConstFloat(r)) => ConstFloat(l / r)
      case (acc, op ~ next) => op match {
        case "*" => Bop(Times(), acc, next)
        case "/" => Bop(Div(), acc, next)
      }
    }
  }

  def as: Parser[Expr] = md ~ rep("+" ~ md | "-" ~ md) ^^ {
    case l ~ list => (l /: list) {
      case (ConstFloat(l), "+" ~ ConstFloat(r)) => ConstFloat(l + r)
      case (ConstFloat(l), "-" ~ ConstFloat(r)) => ConstFloat(l - r)
      case (acc, op ~ next) => op match {
        case "+" => Bop(Plus(), acc, next)
        case "-" => Bop(Minus(), acc, next)
      }
    }
  }
  
  def expr: Parser[Expr] = as
  
  def wloop: Parser[Stmt] =  ("while" ~> "(" ~> expr <~ ")") ~ stmt ^^ {
    case cond ~ body => While(cond, body)
  }
  
  def discard: Parser[Stmt] = (expr <~ ";") ^^ Discard
  
  def ifstmt: Parser[Stmt] = ("if" ~> "(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt) ^^ {
    case condition ~ then ~ orelse => If(condition, then, orelse)
  }
  
  def break: Parser[Stmt] = "break;".r ^^^ Break()
  def continue: Parser[Stmt] = "continue;".r ^^^ Continue()
  
  def assign: Parser[Stmt] = (name ~ ("=" ~> expr <~ ";")) ^^ { case Name(id) ~ e => Assign(id, e) }
  
  def stmt: Parser[Stmt] = assign | discard | wloop | ifstmt | break | continue
}
