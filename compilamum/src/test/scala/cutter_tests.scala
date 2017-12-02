import cutter.{Cut}
import ast._

import org.scalatest._

class CuttingSpec extends FlatSpec with Matchers {
  "Cutter" should "flatten basic function calls" in {
    Cut.flatten(Map(("x", Backend())), Frontend())(
      Return(Call(Name("x"), List()))
    ) should matchPattern {case Right(List(
      NetCall(_, Call(Name("x"), List())),
      Return(Name(_))
    )) => }
  }
  
  it should "handle functions in expressions" in {
    Cut.flatten(Map(("x", Backend())), Frontend())(
      Return(Bop(Plus(), Call(Name("x"), List(Name("y"))), Name("y")))
    ) should matchPattern {case Right(List(
      NetCall(_, Call(Name("x"), List(Name("y")))),
      Return(Bop(Plus(), Name(_), Name("y"))
    ))) => }
  }
  
  it should "handle calls within function arguments" in {
    Cut.flatten(Map(("x", Backend())), Frontend())(
      Return(Call(Name("x"), List(Call(Name("x"), List(Name("y"))))))
    ) should matchPattern {case Right(List(
      NetCall(a, Call(Name("x"), List(Name("y")))),
      NetCall(b, Call(Name("x"), List(Name(c)))),
      Return(Name(d))
    )) => }
  }
}
