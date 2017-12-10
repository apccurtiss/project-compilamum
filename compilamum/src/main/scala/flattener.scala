package flattener

import scala.util.Random
import scala.collection.mutable.LinkedHashMap

import cutter.CutError
import ast._

object Flatten {
  def apply(tree: Node): Either[CutError, Program] = tree match {
    case Program(globals) => {
      swap(globals map remold) map Program
    }
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
        case ((key, Call(func, args))) => CallStmt(key, func, args, Set(), "")
        case ((_,x)) => throw new IllegalArgumentException(s"You shouldn't have put ${x} into an extraction!")
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

  // Extracts all call nodes with different locations from the current context
  // and replaces them with a variable name. Then, it returns the new expression
  // with the replacements, and a mapping from variable name to removed expression.
  def extract(tree: Expr): Either[CutError, Extraction] = {
    tree match {
      case Call(id, args) => {
        val argsres = swap(args map extract) map {_.foldLeft(Extraction(Call(id, List()), LinkedHashMap()))({
          (total_extr, other_extr) => total_extr.combine({
            case (Call(x, args), other) => Call(x, other::args)
            case (_, _) => throw new IllegalArgumentException("Don't change the intial value for foldLeft, idiot!")
          })( other_extr )
        })}

        argsres map {_ flatMap {
          tree => val r = genname(); Extraction(Name("__cut_tmp_"++r), LinkedHashMap(("__cut_tmp_var__"++r, tree)))
        }}
      }
      case ListExpr(items) => swap(items map extract) map {_.reduce({
        (total_extr, other_extr) => total_extr.combine({
          case (ListExpr(items), other) => ListExpr(other::items)
          case (start, other) => ListExpr(List(start, other))
        })( other_extr )
      })}
      case DictExpr(items) => ???

      case Bop(op, left, right) => for {
        left <- extract(left)
        right <- extract(right)
      } yield (Extraction(Bop(op, left.oldExpr, right.oldExpr), left.newExprs ++ right.newExprs))
      case Uop(op, expr) => map2(extract(expr))({ Uop(op, _) })

      case Name(id) => Right(Extraction(tree, LinkedHashMap()))
      case _ => Right(Extraction(tree, LinkedHashMap()))
    }
  }


  // This function takes a statement, calls extract on any contained expressions,
  // and returns a list of statements that perform the extraction. It also recursively
  // calls itself on any stmts that the stmt itself contains.
  //
  // TODO: Consider replacing Map with just a list os statements containg the assignments?
  def flatten(stmt: Stmt): Either[CutError, List[Stmt]] = {
    stmt match {
      case If(condition, body, orelse) => flatten(body) flatMap {
        new_body => flatten(orelse) flatMap {
          new_else => {
            extract(condition) map {
              case extr @ Extraction(expr, _) => extr.toStmts ++ List(If(expr, Block(new_body), Block(new_else)))
            }
          }
        }
      }

      case While(condition, body) => flatten(body) flatMap {
        new_body => {
          extract(condition) map {
            case extr @ Extraction(expr, _) => extr.toStmts ++ List(While(expr, Block(new_body)))
          }
        }
      }

      case Block(body) => swap(body map flatten) map (_.flatten)

      case Declare(to, typ, from) => {
        extract(from) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Declare(to, typ, expr))
        }
      }
      case Assign(to, from) => {
        extract(from) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Assign(to, expr))
        }
      }
      case Discard(value) => {
        extract(value) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Discard(expr))
        }
      }
      case Return(value) => {
        extract(value) map {
          case extr @ Extraction(expr, _) => extr.toStmts ++ List(Return(expr))
        }
      }

      case _ => Right(List(stmt))
    }
  }

  // applies flatten to every statement in function bodies.
  def remold(global: Global): Either[CutError, Global] = global match {
    case FuncDecl(loc, typ, name, params, body) => flatten(body) map {
      ls => { FuncDecl(loc, typ, name, params, Block(ls)) }
    }
    case GlobalDecl(loc, to, typ, from) => Right(GlobalDecl(loc, to, typ, from))
    case Import(loc, typ, name, params, jsCode) => Right(Import(loc, typ, name, params, jsCode))
  }
}
