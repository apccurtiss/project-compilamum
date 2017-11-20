abstract class Location
case class Backend() extends Location
case class Frontend() extends Location

abstract class MType
case class Str(value: String) extends MType
case class Num(value: Float) extends MType
case class Bool(value: Boolean) extends MType
case class Ls(value: List[MType]) extends MType
case class Dict(value: Map[Str, MType]) extends MType

abstract class Op
case class Plus() extends Op

abstract class Expr
case class Call(name: Expr, arguments: List[Expr]) extends Expr
case class ListExpr(items: List[Expr]) extends Expr
case class DictExpr(items: Map[Expr, Expr]) extends Expr
case class Bop(op: Op, left: Expr, right: Expr) extends Expr
case class Uop(op: Op, expr: Expr) extends Expr
case class Name(id: String) extends Expr
case class NumConst(value: Float) extends Expr
case class StringConst(value: String) extends Expr
case class BoolConst(value: Boolean) extends Expr

abstract class Stmt
case class Break() extends Stmt
case class Continue() extends Stmt
case class Declare(to: String, mtype: MType, from: Expr) extends Stmt
case class Assign(to: String, from: Expr) extends Stmt
case class Discard(value: Expr) extends Stmt
case class Return(value: Expr) extends Stmt
case class If(condition: Expr, then: List[Stmt], orelse: List[Stmt]) extends Stmt
case class While(condition: Expr, body: List[Stmt]) extends Stmt

abstract class Global
case class Create(at: Location, to: String, mtype: MType, from: Expr) extends Global
case class Function(at: Location, name: String, params: List[(String, MType)], rtype: MType) extends Global
