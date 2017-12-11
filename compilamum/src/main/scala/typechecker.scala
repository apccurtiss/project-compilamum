package typechecker

import compilamum.Erramum
import runtime.{Runtime}
import ast._

object Typecheck {
  def apply(prog : Program): Either[TypeError, Program] = prog match {
    case Program(globals) => {
      val env:Map[String,Typ] = globals.foldLeft(Map[String,Typ]()){(env,globe) => 
        globe match {
          case FuncDecl(_,typ,name,params,_) => env + (name -> FuncType(params map {case (name,typ) => typ},typ))
          case GlobalDecl(_,name,typ,_) => env + (name -> typ)
          case Import(_,typ,name,params,_) => env + (name -> FuncType(params map {case (name,typ) => typ},typ))
        }
      }
      def helper(globes:List[Global]):Either[TypeError, Boolean] = globes match {
        case Nil => Right(true)
        case globe :: tail => typecheckGlobal(globe,env) match {
          case Right(true) => Right(true)
          case Right(false) => Left(TypeError("Typecheck failed for unknown reason"))
          case Left(err) => Left(err)
        }
      }
      helper(globals) match {
        case Right(true) => Right(prog)
        case Right(false) => Left(TypeError("Typecheck failed for unknown reason"))
        case Left(err) => Left(err)
      }
    }
  }
  def typecheckGlobal(n:Global,env:Map[String, Typ]): Either[TypeError, Boolean] = n match {
    case FuncDecl(_,typ,_,params,stmt) => {
      val env2 = params.foldLeft(env){case (envTmp,(name,typ)) =>
        envTmp + (name -> typ)
      }
      typecheckStmt(stmt,env2) match {
        case Right(typ2) if typ == typ2 => Right(true)
        case Right(typ2) => Left(TypeError("Expected $typ got $typ2"))
        case Left(err) => Left(err)
      }
    }
    case GlobalDecl(_,_,typ,expr) => typecheckExpr(expr,env) match {
      case Right(typ2) if typ2 == typ => Right(true)
      case Right(typ2) => Left(TypeError(s"Expected $typ in assignment got $typ2"))
      case Left(err) => Left(err)
    }
    case Import(_,_,_,_,_) => Right(true)
  }
  def typecheckStmt(n: Stmt, env: Map[String,Typ]): Either[TypeError,Typ] = {
    def helper(n:Stmt, env: Map[String,Typ]): Either[TypeError,(Typ,Map[String,Typ])] = n match {
      case Break() | Continue() => Right((Void(),env))
      case Declare(name, typ, expr) => typecheckExpr(expr,env) match {
        case Right(typ2) if typ == typ2 => Right((Void(),env + (name -> typ)))
        case Right(typ2) => Left(TypeError(s"Expected $typ in assignment got $typ2"))
        case Left(err) => Left(err)
      }
      case Assign(name, expr) => typecheckExpr(expr,env) match {
        case Right(typ) if (env contains name) && (env(name) == typ) => Right((Void(),env))
        case Right(typ) if env contains name => Left(TypeError(s"Expected $env(name) got typ"))
        case Right(typ) => Left(TypeError(s"Variable use before assignment: $name"))
        case Left(err) => Left(err)
      }
      case Discard(expr) => typecheckExpr(expr, env) match {
        case Right(typ) => Right((Void(),env))
        case Left(err) => Left(err)
      }
      case Return(expr) => typecheckExpr(expr,env) match {
        case Right(typ) => Right((typ,env))
        case Left(err) => Left(err)
      }
      case If(cond,stmt1,stmt2) => (typecheckExpr(cond,env),helper(stmt1,env),helper(stmt2,env)) match {
        case (Left(err),_,_) => Left(err)
        case (Right(t),_,_) if t != Bool() => Left(TypeError(s"Expected Bool, got $t"))
        case (_,Left(err),_) => Left(err)
        case (_,_,Left(err)) => Left(err)
        case (_,Right((_,e1)),Right((_,e2))) if e1 != e2 => Left(TypeError(s"Non uniform declaration in if statement"))
        case (_,Right((t1,e)),Right((t2,_))) if t1==t2 => Right((t1,e))
        case (_,Right((Void(),_)),Right((t,e))) => Right((t,e))
        case (_,Right((t,e)),_) => Right((t,e))
        case (_,_,_) => Left(TypeError("Type mismatch return in if statement"))
      }
      case While(cond,stmt) => (typecheckExpr(cond,env),helper(stmt,env)) match {
        case (Left(err), _) => Left(err)
        case (Right(t),_) if t != Bool() => Left(TypeError(s"Expected Bool got $t"))
        case (_, Left(err)) => Left(err)
        case (_, Right((t,env))) => Right((t,env))
      }
      case Block(body) => body.foldRight(Right(Void(),env) : Either[TypeError,(Typ,Map[String,Typ])]){
        case (_,Left(err)) => Left(err)
        case (stmt, Right((Void(),env))) => helper(stmt,env)
        case (stmt, Right((typ,env))) => helper(stmt,env) match {
          case Left(err) => Left(err)
          case Right((typ2,env2)) if typ == typ2 => Right((typ,env2))
          case Right((Void(),env2)) => Right((typ,env2))
          case Right((typ2,env2)) => Left(TypeError(s"Type mismatch in block statment, $typ and $typ2"))
        }
      }
    }
    helper(n,env) match {
      case Right((typ,_)) => Right(typ)
      case Left(err) => Left(err)
    }
  }
  def typecheckExpr(n: Expr, env: Map[String,Typ]): Either[TypeError,Typ] = n match {
    case Name(x) => if (env contains x) Right(env(x)) else Left(TypeError(s"Undefined variable: $x"))
    case ConstFloat(_) => Right(Num())
    case ConstBool(_) => Right(Bool())
    case ConstString(_) => Right(Str())
    
    case Bop(op,e1,e2) => op match {
      case Le()|Gt()|Lt()|Ge() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(Num()),Right(Num())) => Right(Bool())
        case (Right(t1),Right(t2)) => Left(TypeError(s"Either mismatched types or non string/Num types: $t1 and $t2"))
        case (e1,e2) => pullError(e1,e2)
      }
      case Equal()|Neq() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(t1),Right(t2)) => Right(Bool())
        case (e1,e2) => pullError(e1,e2)
      }
      case Pow()|Star()|FSlash()|Minus()|Plus() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(Num()),Right(Num())) => Right(Num())
        case (Right(t1),Right(t2)) => Left(TypeError(s"Non-Num types in Math statement: $t1 and $t2"))
        case (e1,e2) => pullError(e1,e2)
      }
      case And()|Or() => (typecheckExpr(e1, env), typecheckExpr(e2, env)) match {
        case (Right(t1),Right(t2)) if t1 == t2 => Right(t1)
        case (Right(t1),Right(t2)) => Left(TypeError(s"Mismatched types: $t1 and $t2"))
        case (e1,e2) => pullError(e1,e2)
      }
    }
    case Uop(op,expr) => typecheckExpr(expr,env) match{
      case Right(Num()) => Right(Num())
      case Right(other) => Left(TypeError(s"Expected Num got $other"))
      case err => err
    }
    case Call(name,args) => if (env contains name) env(name) match {
      case FuncType(argsTyp,ret) => if( argsTyp.length == args.length) FunctionTest(argsTyp zip args, ret, env) else Left(TypeError(s"Arg length and expected arg length are different"))
      case typ => Left(TypeError(s"Expected a function got $typ"))
    } else Left(TypeError(s"Undefined function: $name"))
    case ListExpr(items) => items.foldRight(Right(Ls()):Either[TypeError,Typ]) {
      case (item:Expr,Left(err)) => Left(err)
      case (item:Expr,Right(t)) => typecheckExpr(item,env) match {
        case Right(_) => Right(t)
        case Left(err) => Left(err)
      }
    }
    case DictExpr(items) => ???
    case x => Left(TypeError(s"Unimplemented: $x"))
  }
  
  def pullError(args:Either[TypeError,Typ]*): Either[TypeError,Typ] = args match {
    case Nil => Left(TypeError("Wrong type")) // this is not supposed to be hit. I want it to crash
    case (err @ Left(_)) :: tail => err
    case _ :: tail => pullError(tail: _*)
  }
  def FunctionTest(args:List[(Typ,Expr)],result:Typ,env: Map[String,Typ]): Either[TypeError, Typ] = args match {
    case Nil => Right(result)
    case (typ,expr)::tail => typecheckExpr(expr,env) match{
      case Right(t1) if t1 == typ => FunctionTest(tail,result,env)
      case Right(t1) => Left(TypeError(s"Expected $typ got $t1"))
      case exp => exp
    }
  }
}

case class TypeError(msg: String) extends Erramum

