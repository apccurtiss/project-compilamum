package cutter

import compilamum.ErrorMum
import ast._

object Cut{
  def apply(tree:Node): Either[ErrorMum,Node]={
    Right(tree)
  }
}

case class CutError() extends ErrorMum
