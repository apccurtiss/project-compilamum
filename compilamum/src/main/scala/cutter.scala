package cutter

import compilamum.Erramum
import ast._
import flattener.Flatten
import cacher.Cache

object Cut {
  def apply(tree: Node): Either[Erramum, (Node, Node)] = {
    val key = classify(tree)
    val flat_tree = Flatten(tree)
    val cached_tree = flat_tree map Cache.apply
    
    cached_tree map {case program =>
      val (client, server) = (program.body flatMap cutGlobal) partition {
        case GlobalDecl(Frontend(), _, _, _) => true
        case FuncDecl(Frontend(), _, _, _, _) => true
        case Import(Frontend(), _, _, _, _) => true
        case _ => false
      }
      (Program(client), Program(server))
    }
  }

  def classify(tree: Node): Map[String, Location] = tree match {
    case Program(globals) => globals map {
      case GlobalDecl(loc, name, _, _) => (name, loc)
      case FuncDecl(loc, _, name, _, _) => (name, loc)
      case Import(loc, _, name, _, _) => (name, loc)
    } toMap
    case _ => throw new IllegalArgumentException("The argument to classify must be a Program");
  }
  
  def cutStmts(nodes: List[Stmt]): List[List[Stmt]] = nodes.foldLeft(List(List() : List[Stmt]): List[List[Stmt]]) {
    case (acc, curr: CallStmt) => acc.init :+ (acc.last :+ curr) :+ List()
    case (acc, curr) => acc.init :+ (acc.last :+ curr)
  }
  
  def cutGlobal(node: Global): List[Global] = node match {
    case FuncDecl(loc, typ, name, params, body) => (body match {
      case Block(body) => cutStmts(body)
      case single => cutStmts(List(single))
    }).foldLeft(List(): List[Global]) {
      case (ls, stmts) => {
        val new_stmts = (stmts.init :+ ((stmts.last) match {
          case CallStmt(to, func, args, cached, _) => CallStmt(to, func, args, cached, name ++ ls.length.toString + 1)
          case x => x
        })).toList
        ls :+ FuncDecl(loc, typ, name ++ ls.length.toString, params, Block(new_stmts))
      }
    }
    case x => List(x)
  }
}

case class CutError(msg: String) extends Erramum
