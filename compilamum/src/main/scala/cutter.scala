package cutter

import scala.util.Random
import scala.collection.mutable.LinkedHashMap

import compilamum.Erramum
import ast._

object Cut {
  def apply(tree: Node): Either[Erramum, (Node, Node)] = tree match {
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

  def classify(tree: Node): Map[String, Location] = tree match {
    case Program(globals) => globals map {
      case GlobalDecl(loc, name, _, _) => (name, loc)
      case FuncDecl(loc, _, name, _, _) => (name, loc)
      case Import(loc, _, name, _) => (name, loc)
    } toMap
    case _ => throw new IllegalArgumentException("The argument to classify must be a Program");
  }
  
  // Do I need an abstract class here???
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
        case ((key, Call(func, args))) => NetCall(key, func, args)
      }).toList.reverse
    }
  }
  
  // TODO:
  // THIS IS A HORRID HACK: IZAAK DID THIS
  // Hey, if someone can figure out how to make this less ugly, please do so!
  // It takes a list of eithers and returns the first Left in the list, or
  // a Right of a list of all of the Right values, if there are no Lefts
  // NOT POSSIBLE: This cannot be made generic, so don't try.
  def swap[A,B](ls: List[Either[A,B]]): Either[A, List[B]] = {
    if (ls exists {_.isLeft}) {
      Left(ls.find(_.isLeft).get.left.toOption.get)
    } else {
      Right(ls map {_.toOption.get})
    }
  }
  
  // TODO: Make this generic?
  def mapM[A,B,C](xs:List[A])(f:A=>Either[C,B]) : Either[C, List[B]] = {
    swap(xs.map(f))
  }
  
  // TODO: Make this generic?
  def map2[A](arg: Either[A, Extraction])(f: Expr => Expr): Either[A, Extraction] = {
    arg map (_ map f)
  }
  
  // Yo, this is a hack to avoid passing around an explicit counter
  // or wrapping everything in yet another monad. HELP!
  val gen = new Random(13)
  def genname(): String = {
    gen.alphanumeric.take(10).foldLeft("")((t, o) => {t :+ o})
  }
  
  def extract(key: Map[String, Location], context: Location)(tree: Expr): Either[CutError, Extraction] = {
    val recurse = extract(key, context)(_)
    tree match {
      case Call(id, args) => if ((key contains id) && ((key get id) != Some(context))) {
        val argsres = swap(args map recurse) map {_.foldLeft(Extraction(Call(id, List()), LinkedHashMap()))({
          (total_extr, other_extr) => total_extr.combine({
            case (Call(x, args), other) => Call(x, other::args)
          })( other_extr )
        })}
        
        argsres map {_ flatMap {
          tree => val r = genname(); Extraction(Name("__cut_tmp_"++r), LinkedHashMap(("__cut_tmp_var__"++r, tree)))
        }}
      } else {
        swap(args map recurse) map {_.reduce({
          (total_extr, other_extr) => total_extr.combine({
            case (Call(name, args), other) => Call(name, other::args)
            case (start, other) => Call(id, List(start, other))
          })( other_extr )
        })}
      }
      case ListExpr(items) => swap(items map recurse) map {_.reduce({
        (total_extr, other_extr) => total_extr.combine({
          case (ListExpr(items), other) => ListExpr(other::items)
          case (start, other) => ListExpr(List(start, other))
        })( other_extr )
      })}
      case DictExpr(items) => ???
      
      case Bop(op, left, right) => for {
        left <- recurse(left)
        right <- recurse(right)
      } yield (Extraction(Bop(op, left.oldExpr, right.oldExpr), left.newExprs ++ right.newExprs))
      case Uop(op, expr) => map2(recurse(expr))({ Uop(op, _) })

      case Name(id) => if ((key contains id) && ((key get id) != Some(context))) {
        Left(CutError(s"Found the global variable `${id}` in the wrong location context"))
      } else {
        Right(Extraction(tree, LinkedHashMap()))
      }
      case _ => Right(Extraction(tree, LinkedHashMap()))
    }
  }
  
  def flatten(key: Map[String, Location], context: Location)(stmt: Stmt): Either[CutError, List[Stmt]] = {
    val recurse = flatten(key, context)(_)
    stmt match {
      case If(condition, body, orelse) => recurse(body) flatMap {
        new_body => recurse(orelse) flatMap {
          new_else => {
            extract(key, context)(condition) map {
              case extr @ Extraction(expr, _) => extr.toStmts ++ List(If(expr, Block(new_body), Block(new_else)))
            }
          }
        }
      }
      
      case While(condition, body) => recurse(body) flatMap {
        new_body => {
          extract(key, context)(condition) map {
            case extr @ Extraction(expr, _) => extr.toStmts ++ List(While(expr, Block(new_body)))
          }
        }
      }
      
      case Block(body) => swap(body map recurse) map (_.flatten)
      
      case Declare(to, typ, from) => {
        extract(key, context)(from) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Declare(to, typ, expr))
        }
      }
      case Assign(to, from) => {
        extract(key, context)(from) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Assign(to, expr))
        }
      }
      case Discard(value) => {
        extract(key, context)(value) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Discard(expr))
        }
      }
      case Return(value) => {
        extract(key, context)(value) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Return(expr))
        }
      }
      
      case _ => Right(List(stmt))
    }
  }
}

case class CutError(msg: String) extends Erramum
