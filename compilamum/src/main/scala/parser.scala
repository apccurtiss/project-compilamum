package parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional
import compilamum.ErrorMum

import ast._

case class ParseError(line: Int, column: Int, msg: String) extends ErrorMum

object Parse extends RegexParsers {
  override def failure(msg: String) = "" ~> super.failure(msg)
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\n\r\f]+".r

  def apply(code: String): Either[ErrorMum, Program] = {
    parse(phrase(global), code) match {
      case NoSuccess(msg, next) => Left(ParseError(next.pos.line-1, next.pos.column-1, msg))
      case Success(result, next) => Right(result)
    }
  }

  // For testing
  def parseBlock(code: String): Either[ParseError, List[Stmt]] = {
    parse(phrase(rep(stmt)), code) match {
      case NoSuccess(msg, next) => Left(ParseError(next.pos.line-1, next.pos.column-1, msg))
      case Success(result, next) => Right(result)
    }
  }

  ////////////
  // GLOBAL //
  ////////////

  def global: Parser[Program] = rep(function | globalDecl) ^^ Program

  def globalDecl: Parser[GlobalDecl] = location ~ name ~ (":" ~> typ) ~ ("=" ~> expr <~ semi) ^^ {
    case loc ~ Name(x) ~ typ ~ expr => GlobalDecl(loc, x, typ, expr)
  }

  def importStmt: Parser[Global] = ???

  def function: Parser[FuncExpr] = location ~ name ~ ("(" ~> params <~ ")") ~ (":" ~> typ) ~ stmt ^^ {
    case l ~ Name(id) ~ p ~ t ~ b => FuncExpr(l, t, id, p, b)
  }

  def location: Parser[Location] = ("frontend" ^^^ Frontend()) | ("backend" ^^^ Backend())

  ////////////////
  // STATEMENTS //
  ////////////////

  def stmt: Parser[Stmt] = ifStmt | whileStmt | single <~ semi | block |
    failure("Not a valid statement.")

  def block: Parser[Block] = ("{" ~> rep(stmt) <~"}") ^^ { case ls => Block(ls) }

  def ifStmt: Parser[If] = ("if" ~> "(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt) ^^ {
    case condition ~ body ~ orelse => If(condition, body, orelse)
  }

  def whileStmt: Parser[While] =  ("while" ~> "(" ~> expr <~ ")") ~ stmt ^^ {
    case cond ~ body => While(cond, body)
  }

  def single: Parser[Stmt] = declare | assign | discard | returnStmt |
    "break" ^^^ Break() | "continue" ^^^ Continue()

  def declare: Parser[Declare] = ("let" ~> name) ~ (":" ~> typ) ~ ("=" ~> expr) ^^ {
    case Name(id) ~ t ~ e => Declare(id, t, e)
  }

  def assign: Parser[Assign] = (name ~ ("=" ~> expr)) ^^ {
    case Name(id) ~ e => Assign(id, e)
  }

  def discard: Parser[Discard] = expr ^^ Discard

  def returnStmt: Parser[Return] = ("return" ~> expr) ^^ Return

  /////////////////
  // EXPRESSIONS //
  /////////////////

  // Order of these matters! call must come before name, for example
  def expr: Parser[Expr] = addSubExpr

  def boolExpr: Parser[Expr] = ???

  def multDivExpr: Parser[Expr] = atom ~ rep("*" ~ atom | "/" ~ atom) ^^ {
    case l ~ list => (l /: list) {
      case (ConstFloat(l), "*" ~ ConstFloat(r)) => ConstFloat(l * r)
      case (ConstFloat(l), "/" ~ ConstFloat(r)) => ConstFloat(l / r)
      case (acc, op ~ next) => op match {
        case "*" => Bop(Star(), acc, next)
        case "/" => Bop(FSlash(), acc, next)
      }
    }
  }

  def addSubExpr: Parser[Expr] = multDivExpr ~ rep("+" ~ multDivExpr | "-" ~ multDivExpr) ^^ {
    case l ~ list => (l /: list) {
      case (ConstFloat(l), "+" ~ ConstFloat(r)) => ConstFloat(l + r)
      case (ConstFloat(l), "-" ~ ConstFloat(r)) => ConstFloat(l - r)
      case (acc, op ~ next) => op match {
        case "+" => Bop(Plus(), acc, next)
        case "-" => Bop(Minus(), acc, next)
      }
    }
  }

  def expExpr: Parser[Expr] = ???

  def unaryExpr: Parser[Expr] = ???

  def atom: Parser[Expr] = const | call | name | "(" ~> expr <~ ")" | failure("Unexpected end of line.")

  def call: Parser[Expr] = name ~ ("("~> repsep(expr, ",") <~")") ^^ {
    case n ~ ls => Call(n, ls)
  }

  def list: Parser[Expr] = ???

  def dict: Parser[Expr] = ???

  def equalOp: Parser[Expr] = ???

  def boolOp: Parser[Expr] = ???

  def addSubOp: Parser[Expr] = ???

  def multDivOp: Parser[Expr] = ???

  def expOp: Parser[Expr] = ???

  def unaryOp: Parser[Expr] = ???

  //////////////
  // Literals //
  //////////////

  def const: Parser[Expr] = bool | string | number

  def bool: Parser[ConstBool] = ("True" | "False") ^^ { b => ConstBool(b == "True") }

  def string: Parser[ConstString] = ("\"" ~> "[^\"]*".r <~ "\"") ^^ ConstString

  def number: Parser[ConstFloat] = "\\d+(:?\\.\\d*)?|\\.\\d+".r ^^ { n => ConstFloat(n.toDouble) }

  ///////////
  // Other //
  ///////////

  def args: Parser[List[Expr]] = ???

  def params: Parser[Map[String,Typ]] = repsep(name ~ (":" ~> typ), ",") ^^ {
    case p => p map { case Name(id) ~ t => (id, t) } toMap
  }

  def name: Parser[Name] = not(keyword) ~> "[\\w_][\\w_\\d]*".r ^^ Name

  def typ: Parser[Typ] = ( "String" ^^^ Str()
                           | "Number" ^^^ Num()
                           | "List" ^^^ Ls()
                           | "Dictionary" ^^^ Dict()
                           | "Boolean" ^^^ Bool()
                           | "Void" ^^^ Bool()
                           | failure("Invalid type") )

  def javascript: Parser[Expr] = ???

  ///////////
  // Misc. //
  ///////////
  def keyword: Parser[String] = ( "if" | "else" | "while" | "break" | "continue"
                              | "function" | "return" | "frontend" | "backend"
                              | "let" )

  def semi: Parser[String] = ";" | failure("Expected semicolon.")
}
