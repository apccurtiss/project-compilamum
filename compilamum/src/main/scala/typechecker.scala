package typechecker

import compilamum.Erramum
import ast._

object Typecheck {
  def apply(tree: Node): Either[TypeError, Node] = Right(tree) // TODO: This
}

case class TypeError() extends Erramum
