import parser.{Parse,ParseError}
import generater.{Generate,GenerateError}
import ast._

import org.scalatest._

class GeneratingSpec extends FlatSpec with Matchers {
  "Generate" should "generate floats" in {
    Generate(ConstFloat(1.0)) should be (Right("1.0"))
  }

  it should "generate strings" in {
    Generate(ConstString("Hello world")) should be (Right("\"Hello world\""))
    Generate(ConstString("\\\"Hello world\\\"")) should be (Right("\"\\\"Hello world\\\"\""))
  }

  it should "generate binary operator exprs" in {
    Generate(Bop(Plus(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("1.0 + 1.0"))
    Generate(Bop(Minus(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("1.0 - 1.0"))
    Generate(Bop(Star(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("1.0 * 1.0"))
    Generate(Bop(FSlash(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("1.0 / 1.0"))
  }

  it should "generate calls" in {
    Generate(Call(Name("foo"), List(ConstFloat(1.0), Name("x")))) should be (Right("foo(1.0, x)"))
  }
}
