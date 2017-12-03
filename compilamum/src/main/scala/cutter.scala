package cutter

import compilamum.Erramum
import ast._

object Cut {
  def apply(tree: Node): Either[Erramum, (Node, Node)] = {
    val key = classify(tree)
    tree match {
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

  def classify(tree: Node): Map[String, Location] = tree match {
    case Program(globals) => globals map {
      case GlobalDecl(loc, name, _, _) => (name, loc)
      case FuncDecl(loc, _, name, _, _) => (name, loc)
      case Import(loc, _, name, _) => (name, loc)
    } toMap
    case _ => throw new IllegalArgumentException("The argument to classify must be a Program");
  }
}

case class CutError(msg: String) extends Erramum
