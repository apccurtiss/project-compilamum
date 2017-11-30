package generater

import compilamum.ErrorMum
import ast._

object Generate {
  def apply(tree: Node): Either[ErrorMum, String] = {
    Right(gen(tree))
  }

  def gen(tree: Node): String = tree match {
    case Program(lines) => lines map gen mkString("\n")

    case Call(f, args) => s"${gen(f)}(${args map gen mkString(", ")})"

    case Bop(Plus(), e1, e2) => s"${gen(e1)} + ${gen(e2)}"
    case Bop(Minus(), e1, e2) => s"${gen(e1)} - ${gen(e2)}"
    case Bop(Star(), e1, e2) => s"${gen(e1)} * ${gen(e2)}"
    case Bop(FSlash(), e1, e2) => s"${gen(e1)} / ${gen(e2)}"

    case Name(s) => s
    case ConstString(s) => "\"" + s + "\""
    case ConstFloat(f) => f.toString()
    case _ => ???
  }
}

case class GenerateError() extends ErrorMum
