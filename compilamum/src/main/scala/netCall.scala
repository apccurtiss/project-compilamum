package netCall

import compilamum.ErrorMum
import ast._

object GenNetCall{
  def apply(tree:List[Global]): Either[ErrorMum,Node]={
    ???
  }
}

case class NetCallError() extends ErrorMum
