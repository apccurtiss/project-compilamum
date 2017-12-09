package ast

// for easy typing of the functions
abstract class Node

abstract class Location extends Node
case class Backend() extends Location
case class Frontend() extends Location

abstract class Typ extends Node
case class Str() extends Typ
case class Num() extends Typ
case class Bool() extends Typ
case class Ls() extends Typ
case class Dict() extends Typ
case class Void() extends Typ
case class Any() extends Typ
case class FuncType(args: List[Typ], ret: Typ) extends Typ

// deal with the distinction of things not being unary ops at compile time, not parse time
abstract class Op extends Node
case class Plus() extends Op //"+"
case class Pow() extends Op  //"^"
case class Equal() extends Op //"=="
case class Le() extends Op // "<="
case class Star() extends Op // "*"
case class Gt() extends Op // ">"
case class Neq() extends Op // "!="
case class FSlash() extends Op // "/"
case class And() extends Op // "and"
case class Or() extends Op // "or"
case class Lt() extends Op // "<"
case class Minus() extends Op  // "-"
case class Ge() extends Op // ">="

abstract class Expr extends Node
case class Call(name: String, args: List[Expr]) extends Expr
case class Bop(op: Op, left: Expr, right: Expr) extends Expr
case class Uop(op: Op, expr: Expr) extends Expr
case class ListExpr(items: List[Expr]) extends Expr
case class DictExpr(items: Map[Expr, Expr]) extends Expr

abstract class Literal extends Expr
case class Name(id: String) extends Literal
case class ConstFloat(value: Double) extends Literal
case class ConstString(value: String) extends Literal
case class ConstBool(value: Boolean) extends Literal

abstract class Stmt extends Node
case class Break() extends Stmt
case class Continue() extends Stmt
case class Declare(to: String, typ: Typ, from: Expr) extends Stmt
case class Assign(to: String, from: Expr) extends Stmt
case class Discard(value: Expr) extends Stmt
case class Return(value: Expr) extends Stmt
case class If(condition: Expr, body: Stmt, orelse: Stmt) extends Stmt
case class While(condition: Expr, body: Stmt) extends Stmt
case class Block(body: List[Stmt]) extends Stmt
case class NetCall(to: String, func: String, args: List[Expr], cached: Set[String]) extends Stmt

abstract class Global extends Node
case class FuncDecl(loc: Location, typ: Typ, name: String, params: Map[String,Typ], body: Stmt) extends Global
case class GlobalDecl(loc: Location, to: String, typ: Typ, from: Expr) extends Global
case class Import(loc: Location, typ:Typ, name: String, params: Map[String,Typ], jsCode: String) extends Global

// and there's the program over all:
case class Program(body: List[Global]) extends Node
