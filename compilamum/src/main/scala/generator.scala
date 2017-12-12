package generator

import compilamum.Erramum
import runtime.RuntimeObject
import ast._

object Generate {
  def apply(tree: Node, runtime: List[RuntimeObject]): Either[Erramum, String] = {
    try {
      Right(runtime.foldLeft(""){ case (acc, h) => acc + h.code } + gen(tree))
    }
    catch {
      case err: GenerateError => Left(err)
    }
  }

  def gen(tree: Node): String = tree match {
    case Program(lines) => lines map gen mkString("\n")

    case GlobalDecl(_, name, _, value) => s"var $name = ${gen(value)};"
    case GlobalFuncDecl(_, _, name, params, body) => {
      s"function $name(${params map(_._1) mkString(", ")}) {\n${gen(body)}\n}"
    }
    case Import(_, _, name, _, jsCode) => s"var $name = $jsCode"


    case FuncDecl(_, name, params, body) => {
      s"function $name(${params map(_._1) mkString(", ")}) {\n${gen(body)}\n}"
    }
    case Block(stmts) => stmts map { line => s"${gen(line)};" } mkString("\n")
    case Discard(stmt) => gen(stmt)
    case Declare(x, _, v) => s"var $x = ${gen(v)}"
    case Assign(x, v) => s"$x = ${gen(v)}"
    case Return(expr) => s"return ${gen(expr)}"

    case Call(f, args) => s"${f}(${args map gen mkString(", ")})"
    // case class CallStmt(to: String, func: String, args: List[Expr], cached: Set[String], returnto: String) extends Stmt
    case CallStmt(to, func, args, cached, ret) => s"return get_backend_function('$func', ${args map gen mkString(", ")})($ret)"

    // TODO(alex) Implement operator precedence
    case Bop(Plus(), e1, e2) => s"(${gen(e1)} + ${gen(e2)})"
    case Bop(Minus(), e1, e2) => s"(${gen(e1)} - ${gen(e2)})"
    case Bop(Star(), e1, e2) => s"(${gen(e1)} * ${gen(e2)})"
    case Bop(FSlash(), e1, e2) => s"(${gen(e1)} / ${gen(e2)})"

    case Name(s) => s
    case ConstString(s) => "\"" + s + "\""
    case ConstFloat(f) => f.toString()
    case unknown => throw GenerateError(s"Can't generate code for $unknown")
  }
}

case class GenerateError(msg: String) extends Erramum
