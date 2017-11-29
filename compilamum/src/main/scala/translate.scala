package translate

import compilamum.ErrorMum
import ast._

object Translate{
  def apply(tree:Node): Either[ErrorMum,Node]={
    Right(tree)
  }
}

case class TranslateError() extends ErrorMum
