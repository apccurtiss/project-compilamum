package typechecker

import compilamum.Erramum
import ast._

object Typecheck {
  def apply(tree: Node): Either[TypeError, Node] = typecheck(tree, Map()) match {
    case err @ Left(_) => err
    case _ => Right(tree)
  }

  def typecheck(n: Node, env: Map[String,Typ]): Either[TypeError,Typ] = n match {
    case Name(x) => if (env contains x) Right(env(x)) else Left(TypeError(s"Undefined variable: $x"))
    case ConstFloat(f) => Right(Num())
    case ConstBool(f) => Right(Bool())
    case ConstString(f) => Right(Str())

    case Discard(expr) => typecheck(expr, env)

    case Bop(_, e1, e2) => (typecheck(e1, env), typecheck(e2, env)) match {
      case (Right(t1), Right(t2)) if t1 == t2 => Right(t1)
      case (Right(t1), Right(t2)) => Left(TypeError(s"Mismatched types: $t1 and $t2"))
      case (err @ Left(_), _) => err
      case (_, err @ Left(_)) => err
    }

    case Call(fname, args) => typecheck(Name(fname), env) flatMap {
      case FuncType(argts, ret) => if (args.length != argts.length) Left(TypeError(s"Expected ${argts.length} arguments, got ${args.length}")) else
        (args zip argts).foldLeft(Right(ret): Either[TypeError,Typ]) {
          case (acc, (arg, argt)) => typecheck(arg, env + (fname -> ret)) flatMap { _ => acc }
        }
      case t => Left(TypeError(s"Cannot call type $t"))
    }

    case Program(stmts) => stmts.foldLeft(Right(env): Either[TypeError,Map[String, Typ]]) {
      case (err @ Left(_), _) => err
      case (Right(env), stmt @ Import(_, _, _, _)) => ???
      case (Right(env), stmt @ FuncDecl(_, t, x, p, b)) => typecheck(b, env ++ p + (x -> t)) map { _ => env + (x -> FuncType(p.values.toList, t)) }
      case (Right(env), stmt @ GlobalDecl(_, x, typ, v)) => typecheck(v, env) match {
          case Left(err) => Left(err)
          case Right(`typ`) => Right(env + (x -> typ))
          case Right(badtyp) => Left(TypeError(s"Expected $typ, got $badtyp."))
      }
      case _ => ???
    } map { _ => Void() }

    case Block(stmts) => stmts.foldLeft(Right(env): Either[TypeError,Map[String, Typ]]) {
      case (err @ Left(_), _) => err
      case (Right(env), stmt @ Declare(x, typ, v)) => typecheck(v, env) match {
          case Left(err) => Left(err)
          case Right(`typ`) => Right(env + (x -> typ))
          case Right(typp) => Left(TypeError(s"Type mismatch: Expected $typ, got $typp."))
      }
      case (Right(env), stmt) => typecheck(stmt, env) map { _ => env }
      case _ => ???
    } map { _ => Void() }

    case x => Left(TypeError(s"Unimplemented: $x"))
  }
}

case class TypeError(msg: String) extends Erramum
