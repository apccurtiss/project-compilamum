package typechecker

import compilamum.Erramum
import ast._

object Typecheck {
  def apply(tree: Node): Option[Erramum] = None // TODO: This
}

case class TypeError() extends Erramum
