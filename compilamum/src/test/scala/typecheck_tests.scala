import typechecker.{Typecheck,TypeError}
import ast._

import org.scalatest._

class TypecheckSpec extends FlatSpec with Matchers {
  "Typecheck" should "allow constant expressions" in {
    Typecheck(ConstString("Hello world!")) should be (Right(ConstString("Hello world!")))
    Typecheck(ConstFloat(1.0)) should be (Right(ConstFloat(1.0)))
    Typecheck(ConstBool(true)) should be (Right(ConstBool(true)))
  }

  it should "allow valid binary expressions" in {
    Typecheck(Bop(Minus(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right(Bop(Minus(), ConstFloat(1.0), ConstFloat(1.0))))
    Typecheck(Bop(Plus(), ConstString("asdf"), ConstString("asdf"))) should be (Right(Bop(Plus(), ConstString("asdf"), ConstString("asdf"))))
  }

  it should "disallow invalid binary expressions" in {
    Typecheck(Bop(Minus(), ConstFloat(1.0), ConstBool(false))) shouldBe a[Left[TypeError,_]]
    Typecheck(Bop(Plus(), ConstBool(true), ConstString("asdf"))) shouldBe a[Left[TypeError,_]]
  }
}
