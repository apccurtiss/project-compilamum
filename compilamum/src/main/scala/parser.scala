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

<<<<<<< HEAD
  def global: Parser[Node] = global_decl | import_stmt | function
  def global_decl: Parser[Node] = (location ~ typ ~ name ~ "=" ~> expr) <~ ";"
  def import_stmt: Parser[Node] = ("import" ~> location ~ "as" ~> name ~ ("(" ~> params <~ ")")) <~ ";" ^^ ???
  def function: Parser[Node] = location ~ typ ~ name ~ ("(" ~> params <~ ")") ~ stmt ^^ ???
  def location: Parser[Location] = ???

  def stmt: Parser[Node] = if_stmt | while_stmt | single <~ ";" | stmts
  def stmts: Parser[Node] = ???
  def if_stmt: Parser[Node] = ("if" ~> "(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt) ^^ {
    case condition ~ then ~ orelse => If(condition, then, orelse)
  }
  def while_stmt: Parser[Node] =  ("while" ~> "(" ~> expr <~ ")") ~ stmt ^^ {
    case cond ~ body => While(cond, body)
  }
  def single: Parser[Node] = ( declare
    | assign
    | discard
    | return_stmt
    | "break;".r ^^^ Break()
    | "continue;".r ^^^ Continue() )
=======
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
>>>>>>> 83bb92e2e57522dfd3a579a964e00ffb647b5e71

  def declare: Parser[Node] = ("let" ~> name) ~ (":" ~> typ) ~ ("=" ~> expr <~ ";") ^^ {
    case Name(id) ~ t ~ e => Declare(id, t, e)
  }
  def assign: Parser[Node] = (name ~ ("=" ~> expr <~ ";")) ^^ { case Name(id) ~ e => Assign(id, e) }
  def discard: Parser[Node] = (expr <~ ";") ^^ Discard
  def return_stmt: Parser[Node] = ("return" ~> expr <~ ";") ^^ Return

  def expr: Parser[Node] = ??? //bool_expr [ equal_op ~ bool_expr ]
  def bool_expr: Parser[Node] = ???
  def p_m_expr: Parser[Node] = m_d_expr ~ rep("+" ~ md | "-" ~ md) ^^ {
    case l ~ list => (l /: list) {
      case (ConstFloat(l), "+" ~ ConstFloat(r)) => ConstFloat(l + r)
      case (ConstFloat(l), "-" ~ ConstFloat(r)) => ConstFloat(l - r)
      case (acc, op ~ next) => op match {
        case "+" => Bop(Plus(), acc, next)
        case "-" => Bop(Minus(), acc, next)
      }
    }
  }
<<<<<<< HEAD
  def m_d_expr: Parser[Node] = atom ~ rep("*" ~ atom | "/" ~ atom) ^^ {
    case l ~ list => (l /: list) {
      case (ConstFloat(l), "*" ~ ConstFloat(r)) => ConstFloat(l * r)
      case (ConstFloat(l), "/" ~ ConstFloat(r)) => ConstFloat(l / r)
      case (acc, op ~ next) => op match {
        case "*" => Bop(Times(), acc, next)
        case "/" => Bop(Div(), acc, next)
      }
    }
  }
  def exp_expr: Parser[Node] = ???
  def unary_expr: Parser[Node] = ???
  def atom: Parser[Node] = const | name | "(" ~> expr <~ ")" | failure("Unexpected end of line.")
  def call: Parser[Node] = ???
  def list: Parser[Node] = ???
  def dict: Parser[Node] = ???
  def equal_op: Parser[Op] = ???
  def bool_op: Parser[Op] = ???
  def p_m_op: Parser[Op] = ???
  def m_d_op: Parser[Op] = ???
  def exp_op: Parser[Op] = ???
  def unary_op: Parser[Op] = ???

  def const: Parser[Node] = bool | string | number
  def bool: Parser[Node] = "(True)|(False)".r ^^ { b => ConstBool(b == "True") }
  def string: Parser[Node] = ("\"" ~> "[^\"]*".r <~ "\"") ^^ { s => ConstString(s) }
  def number: Parser[Node] = "\\d+(:?\\.\\d*)?|\\.\\d+".r ^^ { n => ConstFloat(n.toDouble) }

  def args: Parser[Node] = ???
  def params: Parser[Node] = ???
  def name: Parser[Node] = not("break" | "continue") ~> "[\\w_][\\w_\\d]*".r ^^ { x => Name(x) }
  def typ: Parser[Typ] = ( "String" ^^^ Str()
    | "Number" ^^^ Num()
    | "List" ^^^ Ls()
    | "Dictionary" ^^^ Dict()
    | "Boolean" ^^^ Bool()
    | failure("Invalid type") )

=======
  
  def expr: Parser[Expr] = as
>>>>>>> 83bb92e2e57522dfd3a579a964e00ffb647b5e71
}
