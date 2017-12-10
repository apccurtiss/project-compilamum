package typechecker

import compilamum.Erramum
import runtime.{Runtime}
import ast._

object Typecheck {
  def apply(prog: Program): Either[TypeError, Program] = prog match {
    case Program(globals) => {
      ???
      // globals.foldLeft() {
      //   case (Left(err), _) => Left(err)
      //   case (Right(env), GlobalDecl(???)) => ???
      //   case (Right(env), glob) => typecheckGlobal(glob, env)
      // }
      // val env: Map[String, Typ] = globals.foldLeft(Map[String, Typ]()) {
      //   (env,globe) => globe match {
      //     case FuncDecl(_,typ,name,params,_) => env + (name -> FuncType(params map { case (name,typ) => typ }, typ))
      //     case GlobalDecl(_,name,typ,_) => env + (name -> typ)
      //     case Import(_,typ,name,params,_) => env + (name -> FuncType(params map { case (name,typ) => typ }, typ))
      //   }
      // }

      // def helper(globes: List[Global]): Either[TypeError, Boolean] = globes match {
      //   case Nil => Right(true)
      //   case globe :: tail => globe match {
      //     case FuncDecl(_, typ, _, params, stmt) => {
      //       val env2 = env ++ params
      //       ???
      //     }
      //     case GlobalDecl(_,_,typ,stmt) => ???
      //     case Import(_,_,_,_,_) => helper(tail)
      //   }
      // }
      //
      // helper(globals) match {
      //   case Right(true) => Right(prog)
      //   case Right(false) => Left(TypeError("Typecheck failed for unknown reason"))
      //   case Left(err) => Left(err)
      // }
    }
  }

  def typecheckStmt(n: Stmt, env: Map[String,Typ]): Either[TypeError, Boolean] = n match {
    case _ => ???
  }

  def typecheckGlobal(n: Global, env: Map[String, Typ]): Either[TypeError, Boolean] = n match {
    case FuncDecl(_, typ, _, params, body) => typecheckStmt(body, env ++ params) flatMap {
      // case `typ` => Right()
      case badtyp => Left(TypeError(s"Expected type $typ, got $badtyp"))
    }
    case _ => ???
  }

  def typecheckExpr(n: Node, env: Map[String,Typ]): Either[TypeError,Typ] = n match {
    case Name(x) => if (env contains x) Right(env(x)) else Left(TypeError(s"Undefined variable: $x"))
    case ConstFloat(_) => Right(Num())
    case ConstBool(_) => Right(Bool())
    case ConstString(_) => Right(Str())

    case Bop(op,e1,e2) => op match {
      case Le()|Gt()|Lt()|Ge() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(Num()),Right(Num())) => Right(Bool())
        case (Right(t1),Right(t2)) => Left(TypeError(s"Either mismatched types or non string/Num types: $t1 and $t2"))
        case (e1,e2) => pullError(e1,e2)
      }

      case Equal()|Neq() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(t1),Right(t2)) => Right(Bool())
        case (e1,e2) => pullError(e1,e2)
      }

      case Pow()|Star()|FSlash()|Minus()|Plus() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(Num()),Right(Num())) => Right(Num())
        case (Right(t1),Right(t2)) => Left(TypeError(s"Non-Num types in Math statement: $t1 and $t2"))
        case (e1,e2) => pullError(e1,e2)
      }

      case And()|Or() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(t1),Right(t2)) if t1 == t2 => Right(t1)
        case (Right(t1),Right(t2)) => Left(TypeError(s"Mismatched types: $t1 and $t2"))
        case (e1,e2) => pullError(e1,e2)
      }
    }

    case Uop(op,expr) => typecheckExpr(expr,env) match{
      case Right(Num()) => Right(Num())
      case Right(other) => Left(TypeError(s"Expected Num got $other"))
      case err => err
    }

    case Call(name,args) => if (env contains name) env(name) match {
      case FuncType(argsTyp,ret) => if( argsTyp.length == args.length) FunctionTest(argsTyp zip args, ret, env) else Left(TypeError(s"Arg length and expected arg length are different"))
      case typ => Left(TypeError(s"Expected a function got $typ"))
    } else Left(TypeError(s"Undefined function: $name"))

    case ListExpr(items) => ???

    case DictExpr(items) => ???

    case x => Left(TypeError(s"Unimplemented: $x"))
  }

  def pullError(args:Either[TypeError,Typ]*): Either[TypeError,Typ] = args match {
    case Nil => ??? // this is not supposed to be hit. I want it to crash
    case (err @ Left(_)) :: tail => err
    case _ :: tail => pullError(tail: _*)
  }

  def FunctionTest(args: List[(Typ,Expr)], result: Typ, env: Map[String,Typ]): Either[TypeError, Typ] = args match {
    case Nil => Right(result)
    case (typ,expr) :: tail => typecheckExpr(expr, env) match{
      case Right(t1) if t1 == typ => FunctionTest(tail, result, env)
      case Right(t1) => Left(TypeError(s"Expected $typ got $t1"))
      case exp => exp
    }
  }
}

case class TypeError(msg: String) extends Erramum
/*
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
case class Import(loc: Location, jsCode: String, name: String, params: Map[String,Typ]) extends Global

// and there's the program over all:
case class Program(body: List[Global]) extends Node
*/
