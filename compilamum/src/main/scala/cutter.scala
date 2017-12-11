package cutter

import compilamum.Erramum
import ast._
import flattener.Flatten
import cacher.Cache

object Cut {
  def apply(tree: Node): Either[Erramum, (Node, Node)] = {
    val flat_tree = Flatten(tree)

    flat_tree map {
      case program => {
        val (client, server) = (program.body flatMap cutGlobal) partition {
          case GlobalDecl(Frontend(), _, _, _) => true
          case GlobalFuncDecl(Frontend(), _, _, _, _) => true
          case Import(Frontend(), _, _, _, _) => true
          case _ => false
        }
        (Program(client), Program(server))
      }
    }
  }

  def cutStmts(nodes: List[Stmt]): List[List[Stmt]] = nodes.foldLeft(List(List()): List[List[Stmt]]) {
    case (acc, curr: CallStmt) => acc.init :+ (acc.last :+ curr) :+ List()
    case (acc, Block(ls)) => {
      val newls = cutStmts(ls)
      (acc.init :+ (acc.last ++ newls.head)) ++ newls.tail
    }
    case (acc, curr) => acc.init :+ (acc.last :+ curr)
  }

  def cutGlobal(node: Global): List[Global] = node match {
    case GlobalFuncDecl(loc, typ, name, params, Block(body)) => {
      val (_, f) = cutStmts(body).foldLeft((0, (x: List[Stmt]) => x)) {
        case ((n, f), body_segment) => {
          val newfunc = (next: List[Stmt]) => {
            val h = body_segment.init
            val m = FuncDecl(typ, name ++ n.toString, List(("asdf", Void())), Block(next))
            val t = (body_segment.last match {
              case CallStmt(to, func, args, cached, _) => {
                CallStmt(to, func, args, cached, name ++ (n).toString)
              }
              case x => x
            })
            val newbody = if (next.length == 0) h :+ t else h :+ m :+ t
            f(newbody)
          }

          (n+1, newfunc)

          // println(body_segment)
          // val new_stmts = body_segment.init :+ (body_segment.last match {
          //   case CallStmt(to, func, args, cached, _) => {
          //     CallStmt(to, func, args, cached, name ++ (n + 1).toString)
          //   }
          //   case x => x
          // })
          // val newname = if (n != 0) name ++ n.toString else name
          // val newfunc = FuncDecl(typ, newname, List(("in", Void())), Block(new_stmts))
          // val newbody = body match {
          //   case (Nil) => List(newfunc)
          //   case (Nil :+ l) => List(newfunc, l)
          //   case (i :+ l) => i :+ newfunc :+ l
          // }
          // (n + 1, newbody, newf)
        }
      }
      List(GlobalFuncDecl(loc, typ, name, params, Block(f(List()))))
    }
    case x => List(x)
  }
  // def cutGlobal(node: Global): List[Global] = node match {
  //   case GlobalFuncDecl(loc, typ, name, params, Block(body)) => cutStmts(body).foldLeft(List[Global]()) {
  //     case (acc, List()) => acc
  //     case (acc, body_segment) => {
  //       println(body_segment)
  //       val new_stmts = body_segment.init :+ (body_segment.last match {
  //         case CallStmt(to, func, args, cached, _) => {
  //           CallStmt(to, func, args, cached, name ++ (acc.length + 1).toString)
  //         }
  //         case x => x
  //       })
  //       val newname = if (acc.length != 0) name ++ acc.length.toString else name
  //       acc :+ GlobalFuncDecl(loc, typ, newname, params, Block(new_stmts))
  //     }
  //   }
  //   case x => List(x)
  // }
}

case class CutError(msg: String) extends Erramum
