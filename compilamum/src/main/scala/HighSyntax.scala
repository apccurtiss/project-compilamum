abstract class Location extends Node
case class Backend() extends Location
case class Frontend() extends Location

abstract class MType extends Node
case class Str(value: String) extends MType
case class Num(value: Float) extends MType
case class Bool(value: Boolean) extends MType
case class Ls(value: List[MType]) extends MType
case class Dict(value: Map[Str, MType]) extends MType

// deal with the distinction of things not being unary ops at compile time, not parse time
abstract class Op extends Node
case class Plus() extends Op //"+"
case class Pow() extends Op  //"^" 
case class Equal() extends Op //"==" 
case class Leq() extends Op // "<=" 
case class Times() extends Op // "*" 
case class Grtr() extends Op // ">" 
case class Neq() extends Op // "!=" 
case class Div() extends Op // "/"
case class And() extends Op // "and" 
case class Or() extends Op // "or"
case class Lstn() extends Op // "<"
case class Minus() extends Op  // "-"
case class Greq() extends Op // ">="

abstract class Expr extends Node
case class Call(name: Expr, args: List[Expr]) extends Expr
case class Bop(op: Op, left: Expr, right: Expr) extends Expr
case class Uop(op: Op, expr: Expr) extends Expr

// This may be useful to have in case of base cases
abstract class Atom extends Expr
case class Name(id: String) extends Atom
case class NumConst(value: Float) extends Atom
case class StringConst(value: String) extends Atom
case class BoolConst(value: Boolean) extends Atom
case class ListExpr(items: List[Expr]) extends Atom
case class DictExpr(items: Map[Expr, Expr]) extends Atom

abstract class Stmt extends Node
case class Break() extends Stmt
case class Continue() extends Stmt
case class Declare(to: String, mtype: MType, from: Expr) extends Stmt
case class Assign(to: String, from: Expr) extends Stmt
case class Discard(value: Expr) extends Stmt
case class Return(value: Expr) extends Stmt
case class If(condition: Expr, then: Stmt, orelse: Stmt) extends Stmt
case class While(condition: Expr, body: Stmt) extends Stmt
case class Stmts(first: Stmt, second: Stmt) extends Stmt

abstract class Global extends Node
case class FuncExpr(loc:Location, mtype: MType, name: String, params: Map[String,MType], body: Stmt) extends Global
case class GlobalDecl(loc:Location, to: String, mtype: MType, from: Expr) extends Global
case class Import(loc:Location, jsCode: String, name:String,params : Map[String,MType]) extends Global

// and there's the program over all:
case class Program(body:List[Global]) extends Node

// for easy typing of the functions
abstract class Node
