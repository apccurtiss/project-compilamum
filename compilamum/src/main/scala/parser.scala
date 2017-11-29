package parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

import ast._

case class ParseError(line: Int, column: Int, msg: String)

object Parseamum extends RegexParsers {
  override def failure(msg: String) = "" ~> super.failure(msg)
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[ParseError, List[Global]] = {
    parse(phrase(global), code) match {
      case NoSuccess(msg, next) => Left(ParseError(next.pos.line-1, next.pos.column-1, msg))
      case Success(result, next) => Right(result)
    }
  }

  // For testing
  def parseBlock(code: String): Either[ParseError, List[Stmt]] = {
    parse(phrase(block), code) match {
      case NoSuccess(msg, next) => Left(ParseError(next.pos.line-1, next.pos.column-1, msg))
      case Success(result, next) => Right(result)
    }
  }
  
  /////////////
  // GENERAL //
  /////////////
  
  
  def keyword: Parser[String] = ( "if" | "else" | "while" | "break" | "continue"
                              | "function" | "return" | "frontend" | "backend"
                              | "let" )
  def name: Parser[Literal] = not(keyword) ~> "[\\w_][\\w_\\d]*".r ^^ { x => Name(x) }
  
  def mtype: Parser[MType] = ( "String" ^^^ Str()
                           | "Number" ^^^ Num()
                           | "List" ^^^ Ls()
                           | "Dictionary" ^^^ Dict()
                           | "Boolean" ^^^ Bool()
                           | failure("Invalid type") )

  ////////////
  // GLOBAL //
  ////////////

  def global: Parser[List[Global]] = rep(function | gdecl | jmport)

  def location: Parser[Location] = ("frontend" ^^^ Frontend()) | ("backend" ^^^ Backend())

  def params: Parser[Map[String,MType]] = rep(name ~ ("," ~> mtype)) ^^ {
    case p => Map[String,MType](
      (
        p map {
          case Name(id) ~ t => (id, t)
        }
      ):_*
    )
  }

  def function: Parser[Global] = location ~ name ~ ("(" ~> params <~ ")") ~ ("->" ~> mtype) ~ stmt ^^ {
    case l ~ Name(id) ~ p ~ t ~ b => FuncExpr(l, t, id, p, b)
  }
  
  def gdecl: Parser[Global] = failure("global delcarations are not implemented yet")

  def jmport: Parser[Global] = failure("imports are not implemented yet")

  ////////////////
  // STATEMENTS //
  ////////////////

  def block: Parser[List[Stmt]] = rep(stmt)
  
  def wloop: Parser[Stmt] =  ("while" ~> "(" ~> expr <~ ")") ~ stmt ^^ {
    case cond ~ body => While(cond, body)
  }
  
  def discard: Parser[Stmt] = (expr <~ ";") ^^ Discard
  
  def ifstmt: Parser[Stmt] = ("if" ~> "(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt) ^^ {
    case condition ~ then ~ orelse => If(condition, then, orelse)
  }
  
  def break: Parser[Stmt] = "break" ~ ";" ^^^ Break()
  def continue: Parser[Stmt] = "continue" ~ ";" ^^^ Continue()
  def ret: Parser[Stmt] = ("return" ~> expr <~ ";") ^^ Return
  
  def assign: Parser[Stmt] = (name ~ ("=" ~> expr <~ ";")) ^^ { case Name(id) ~ e => Assign(id, e) }
  def declare: Parser[Stmt] = ("let" ~> name) ~ (":" ~> mtype) ~ ("=" ~> expr <~ ";") ^^ {
    case Name(id) ~ t ~ e => Declare(id, t, e)
  }
  
  def stmts: Parser[Stmt] = ("{" ~> rep(stmt) <~"}") ^^ { case ls => Stmts(ls) }
  
  def stmt: Parser[Stmt] = assign | discard | wloop | ifstmt | break | continue | declare | ret | stmts | failure("Not a valid statement.")
  
  /////////////////
  // EXPRESSIONS //
  /////////////////
  
  // Order of these matters! call must come before name, for example
  def atom: Parser[Expr] = const | call | name | "(" ~> expr <~ ")" | failure("Unexpected end of line.")

  def const: Parser[Expr] = bool | string | number
  def bool: Parser[Literal] = ("True" | "False") ^^ { b => ConstBool(b == "True") }
  def string: Parser[Literal] = ("\"" ~> "[^\"]*".r <~ "\"") ^^ { s => ConstString(s) }
  def number: Parser[Literal] = "\\d+(:?\\.\\d*)?|\\.\\d+".r ^^ { n => ConstFloat(n.toDouble) }

  def call: Parser[Expr] = name ~ ("("~> rep(expr <~ ",") <~")") ^^ {
    case n ~ ls => Call(n, ls)
  }

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
}
