import generator.{Generate,GenerateError}
import runtime.{Runtime}
import ast._

import org.scalatest._

class GeneratingSpec extends FlatSpec with Matchers {
  def gen = (ast: Node) => Generate(ast, List())
  def genClient = (ast: Node) => Generate(ast, Runtime.client)
  def genServer = (ast: Node) => Generate(ast, Runtime.server)
  "Generate" should "generate floats" in {
    gen(ConstFloat(1.0)) should be (Right("1.0"))
  }

  it should "generate strings" in {
    gen(ConstString("Hello world")) should be (Right("\"Hello world\""))
    gen(ConstString("\\\"Hello world\\\"")) should be (Right("\"\\\"Hello world\\\"\""))
  }

  it should "generate binary operator exprs" in {
    gen(Bop(Plus(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("(1.0 + 1.0)"))
    gen(Bop(Minus(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("(1.0 - 1.0)"))
    gen(Bop(Star(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("(1.0 * 1.0)"))
    gen(Bop(FSlash(), ConstFloat(1.0), ConstFloat(1.0))) should be (Right("(1.0 / 1.0)"))
  }

  it should "generate calls" in {
    gen(Call("foo", List(ConstFloat(1.0), Name("x")))) should be (Right("foo(1.0, x)"))
  }
}
