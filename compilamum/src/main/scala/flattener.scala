package flattener

import scala.util.Random
import scala.collection.mutable.LinkedHashMap

import cutter.CutError
import ast._

object Flatten {
  def apply(tree: Node): Either[CutError, Program] = tree match {
    case Program(globals) => {
      val ignore = ((globals filter {
        case _: Import => true
        case _ => false
      }) map {
        case Import(_,_,name,_,_) => name
      }).toSet + "print"
      swap(globals map {x => remold(x, ignore)}) map Program
    }
  }

  case class Extraction(oldExpr: Expr, newExprs: LinkedHashMap[String, Expr]) {
    def map(f: Expr => Expr): Extraction = Extraction(f(oldExpr), newExprs)
    def flatMap(f: Expr => Extraction): Extraction = {
      f(oldExpr) match { case Extraction(o, n) => Extraction(o, n ++ newExprs) }
    }
    def combine(f: (Expr, Expr) => Expr)(other: Extraction) = {
      Extraction(f(oldExpr, other.oldExpr), newExprs ++ other.newExprs)
    }
    def toStmts(): List[Stmt] = {
      (newExprs map {
        case ((key, Call(func, args))) => CallStmt(key, func, args, Set(), "")
        case ((_,x)) => throw new IllegalArgumentException(s"You shouldn't have put ${x} into an extraction!")
      }).toList.reverse
    }
  }

  def swap[A,B](ls: List[Either[A,B]]): Either[A, List[B]] = {
    ls.foldRight(Right(List()): Either[A, List[B]]) {
      case (h, acc) => acc flatMap { l => h map { n => n :: l } }
    }
  }

  def map2[A](arg: Either[A, Extraction])(f: Expr => Expr): Either[A, Extraction] = {
    arg map (_ map f)
  }

  def genname(): String = {
    val gen = new Random(13)
    gen.alphanumeric.take(10).foldLeft("")((t, o) => {t :+ o})
  }

  // Extracts all call nodes with different locations from the current context
  // and replaces them with a variable name. Then, it returns the new expression
  // with the replacements, and a mapping from variable name to removed expression.
  def extract(tree: Expr, ignore: Set[String]): Either[CutError, Extraction] = {
    val recurse = extract(_:Expr, ignore)
    tree match {
      case Call(id, args) if ignore contains id => Right(Extraction(tree, LinkedHashMap()))
      case Call(id, args) => {
        val argsres = swap(args map recurse) map {
          _.foldLeft(Extraction(Call(id, List()), LinkedHashMap())){
            (total_extr, other_extr) => total_extr.combine {
              case (Call(x, args), other) => Call(x, other :: args)
              case (x, _) => throw new IllegalArgumentException(s"Expected Call, got $x")
            }(other_extr)
          }
        }

        argsres map {
          _ flatMap {
            tree => {
              val name = "asdf"
              Extraction(Name(name), LinkedHashMap((name, tree)))
            }
          }
        }
      }

      case ListExpr(items) => swap(items map recurse) map {
        _.reduce {
          (total_extr, other_extr) => total_extr.combine {
            case (ListExpr(items), other) => ListExpr(other::items)
            case (start, other) => ListExpr(List(start, other))
          }(other_extr)
        }
      }
      case DictExpr(items) => ???

      case Bop(op, left, right) => for {
        left <- recurse(left)
        right <- recurse(right)
      } yield (Extraction(Bop(op, left.oldExpr, right.oldExpr), left.newExprs ++ right.newExprs))
      case Uop(op, expr) => recurse(expr) map { _ map { Uop(op, _) } }

      case Name(id) => Right(Extraction(tree, LinkedHashMap()))
      case _ => Right(Extraction(tree, LinkedHashMap()))
    }
  }


  // This function takes a statement, calls extract on any contained expressions,
  // and returns a list of statements that perform the extraction. It also recursively
  // calls itself on any stmts that the stmt itself contains.
  //
  // TODO: Consider replacing Map with just a list os statements containg the assignments?
  def flatten(stmt: Stmt, ignore: Set[String]): Either[CutError, List[Stmt]] = {
    val recurse = flatten(_:Stmt, ignore)
    stmt match {
      case If(condition, body, orelse) => recurse(body) flatMap {
        new_body => recurse(orelse) flatMap {
          new_else => {
            extract(condition, ignore) map {
              case extr @ Extraction(expr, _) => extr.toStmts ++ List(If(expr, Block(new_body), Block(new_else)))
            }
          }
        }
      }

      case While(condition, body) => recurse(body) flatMap {
        new_body => {
          extract(condition, ignore) map {
            case extr @ Extraction(expr, _) => extr.toStmts ++ List(While(expr, Block(new_body)))
          }
        }
      }

      case Block(body) => swap(body map recurse) map (_.flatten)

      case Declare(to, typ, from) => {
        extract(from, ignore) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Declare(to, typ, expr))
        }
      }
      case Assign(to, from) => {
        extract(from, ignore) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Assign(to, expr))
        }
      }
      case Discard(value) => {
        extract(value, ignore) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Discard(expr))
        }
      }
      case Return(value) => {
        extract(value, ignore) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Return(expr))
        }
      }

      case _ => Right(List(stmt))
    }
  }

  // applies flatten to every statement in function bodies.
  def remold(global: Global, ignore: Set[String]): Either[CutError, Global] = global match {
    case GlobalFuncDecl(loc, typ, name, params, body) => flatten(body, ignore) map {
      ls => { GlobalFuncDecl(loc, typ, name, params, Block(ls)) }
    }
    case x => Right(x)
  }
}
