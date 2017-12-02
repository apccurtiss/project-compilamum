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
    
    case Bop(_, e1, e2) => (typecheck(e1, env), typecheck(e2, env)) match {
      case (Right(t1), Right(t2)) if t1 == t2 => Right(t1)
      case (Right(t1), Right(t2)) => Left(TypeError(s"Mismatched types: $t1 and $t2"))
      case (err @ Left(_), _) => err
      case (_, err @ Left(_)) => err
    }
    case x => Left(TypeError(s"Unimplemented: $x"))
  }
}

case class TypeError(msg: String) extends Erramum
