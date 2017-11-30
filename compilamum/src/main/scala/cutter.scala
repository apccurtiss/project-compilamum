package cutter

import compilamum.ErrorMum
import ast._

object Cut {
  def apply(tree: Node): Either[ErrorMum, (Node, Node)] = tree match {
    case Program(globals) => {
      val (client, server) = globals partition {
        case GlobalDecl(Frontend(), _, _, _) => true
        case FuncDecl(Frontend(), _, _, _, _) => true
        case Import(Frontend(), _, _, _) => true
        case _ => false
      }
      Right((Program(client), Program(server)))
    }
  }
}

case class CutError() extends ErrorMum
