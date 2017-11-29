package flatten

import compilamum.ErrorMum
import ast._


object FlattenNetCall{
  def apply(tree:Node): Either[ErrorMum,Node]={
    Right(tree)
  }
}


case class FlattenError() extends ErrorMum
