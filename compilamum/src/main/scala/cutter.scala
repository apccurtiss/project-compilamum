package cutter

import compilamum.ErrorMum
import ast._

object Cut {
  def apply(tree:Node): Either[ErrorMum, (Node, Node)] = {
    Right((tree, Program(List())))
  }
}

case class CutError() extends ErrorMum
