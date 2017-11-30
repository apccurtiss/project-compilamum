package generater

import compilamum.ErrorMum
import ast._

object Generate {
  def apply(tree: Node): Either[ErrorMum, String] = {
    try {
      Right(gen(tree))
    }
    catch {
      case err: GenerateError => Left(err)
    }
  }

  def gen(tree: Node): String = tree match {
    case Program(lines) => lines map gen mkString("\n")

    case GlobalDecl(_, name, _, value) => s"var $name = ${gen(value)};"
    case FuncDecl(_, _, name, params, body) => {
      s"function $name(${params map(_._1) mkString(", ")}) {\n${gen(body)}\n}"
    }

    case Block(stmts) => stmts map gen map { line => s"$line;" } mkString("\n")
    case Discard(stmt) => gen(stmt)

    case Call(f, args) => s"${gen(f)}(${args map gen mkString(", ")})"

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

case class GenerateError(msg: String) extends ErrorMum
