package netCall

import compilamum.ErrorMum
import ast._

object GenNetCall{
  def apply(tree:Node): Either[ErrorMum,Node]={
    Right(tree)
  }
}

case class NetCallError() extends ErrorMum
