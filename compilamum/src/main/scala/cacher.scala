package cacher

import ast._

object Cache {
  def apply(tree: Program): Program = tree match {
    case Program(globals) => Program(globals map cache)
  }

  // I HAVE NO IDEA HOW TO TEST THIS NEXT BIT
  case class UseCounter(used: Set[String], current: Stmt) {
    def map(f: Stmt => Stmt) = UseCounter(used, f(current))
  }

  // Gets the used vatiables in an expression
  def used_vars(node: Expr): Set[String] = node match {
    case Name(s) => Set(s)

    case Call(n, a) => (a.toSet flatMap used_vars) + n
    case Bop(_, l, r) => used_vars(l) ++ used_vars(r)
    case Uop(_, e) => used_vars(e)
    case ListExpr(items) => items.toSet flatMap used_vars
    case DictExpr(items) => (items.toSeq flatMap {case (k,v) => List(k, v)}).toSet flatMap used_vars

    case _ => Set()
  }

  // Updates the current set of variables that are used after the current one,
  // Which is used to compute what each Netcall needs to preserve after the call.
  // Not aware of globals right now. Also cannot handle while loops.
  def count(used_before: Set[String], node: Stmt): UseCounter = node match {
    case Declare(s, t, from) => UseCounter(used_before - s ++ used_vars(from), node)
    case NetCall(to, func, args, _) => UseCounter(
      used_before - to ++ (args.toSet flatMap used_vars),
      NetCall(to, func, args, used_before)
    )

    case Block(body) => (body reverse).foldLeft( UseCounter(used_before, Block(List())) ) {
      case (UseCounter(before, Block(ls)), current) => count(before, current) map {
        s => Block(ls ++ List(s))
      }
    }
    case While(condition, body) => ??? // HONESTLY I HAVE NO IDEA HOW TO WRITE THIS??
    case If(condition, body, orelse) => count(used_before, Block(List(orelse))) match {
      case UseCounter(before_else, current_else) => count(before_else, Block(List(body))) match {
        case UseCounter(before_body, current_body) => UseCounter(
          before_body ++ used_vars(condition),
          If(condition, current_body, current_else)
        )
      }
    }

    case Return(value) => UseCounter(used_before ++ used_vars(value), node)
    case Discard(value) => UseCounter(used_before ++ used_vars(value), node)
    case Assign(to, from) => UseCounter(used_before ++ used_vars(from), node)

    case x => UseCounter(used_before, x)
  }

  // Runs count on the body. Good times all around.
  def cache(tree: Global): Global = tree match {
    case FuncDecl(loc, typ, name, params, body) => count(Set(), Block(List(body))) match {
      case UseCounter(_, body) => FuncDecl(loc, typ, name, params, body)
    }
    case x => x
  }
}
