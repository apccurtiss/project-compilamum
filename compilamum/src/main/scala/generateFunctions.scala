package generateFunctions

import compilamum.ErrorMum
import ast._

object GenFunc{
  def apply(tree:Node): Either[ErrorMum,Node]={
    Right(tree)
  }
}

case class GenerateError() extends ErrorMum
