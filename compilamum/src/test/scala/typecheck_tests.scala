import typechecker.{Typecheck,TypeError}
import ast._

import org.scalatest._

class TypecheckSpec extends FlatSpec with Matchers {
  "TypecheckExpr" should "allow constant expressions" in {
    Typecheck.typecheckExpr(ConstString("Hello world!"), Map()) should be (Right(Str()))
    Typecheck.typecheckExpr(ConstFloat(1.0), Map()) should be (Right(Num()))
    Typecheck.typecheckExpr(ConstBool(true), Map()) should be (Right(Bool()))
  }

  it should "allow valid binary expressions" in {
    Typecheck.typecheckExpr(Bop(Minus(), ConstFloat(1.0), ConstFloat(1.0)),Map()) should be (Right(Num()))
  }

  it should "disallow invalid binary expressions" in {
    Typecheck.typecheckExpr(Bop(Minus(), ConstFloat(1.0), ConstBool(false)),Map()) shouldBe a [Left[TypeError,_]]
    Typecheck.typecheckExpr(Bop(Plus(), ConstBool(true), ConstString("asdf")),Map()) shouldBe a [Left[TypeError,_]]
  }

  it should "allow variables" in {
    Typecheck.typecheckStmt(Block(List(Declare("x", Num(), ConstFloat(1.0)), Discard(Bop(Plus(), Name("x"), ConstFloat(1.0))))),Map()) should be (Right(true))
  }

  it should "disallow mismatched variable types" in {
    Typecheck.typecheckStmt(Block(List(Declare("x", Num(), ConstFloat(1.0)), Declare("y", Bool(), ConstBool(false)), Discard(Bop(Plus(), Name("x"), Name("y"))))),Map()) shouldBe a [Left[TypeError,_]]
  }
}
