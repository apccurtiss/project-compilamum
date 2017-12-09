import typechecker.{Typecheck,TypeError}
import ast._

import org.scalatest._

class TypecheckSpec extends FlatSpec with Matchers {
  "TypecheckExpr" should "allow constant expressions" in {
    Typecheck.TypecheckExpr(ConstString("Hello world!")) should be (Right(Str()))
    Typecheck.TypecheckExpr(ConstFloat(1.0)) should be (Right(Num()))
    Typecheck.TypecheckExpr(ConstBool(true)) should be (Right(Bool()))
  }

  it should "allow valid binary expressions" in {
    Typecheck.TypecheckExpr(Bop(Minus(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right(Num())))
  }

  it should "disallow invalid binary expressions" in {
    Typecheck(Bop(Minus(), ConstFloat(1.0), ConstBool(false))) shouldBe a[Left[TypeError,_]]
    Typecheck(Bop(Plus(), ConstBool(true), ConstString("asdf"))) shouldBe a[Left[TypeError,_]]
  }
  /*
  it should "handle function type checking" in {
    val prog = Program(
      [
        FuncDecl(
          Frontend(), 
          Num(), 
          "main", 
          Map("arg1"->Bool()), 
          Discard(
            Call(
              "main", 
              [
                ConstBool(false)
              ]
            )
          )
        )
      ]
    )
    Typecheck(prog) should be (prog)
  }
    */
}
