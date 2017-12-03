import flattener.{Flatten}
import ast._

import org.scalatest._

class CuttingSpec extends FlatSpec with Matchers {
  "Cutter" should "flatten basic function calls" in {
    Flatten.flatten(Map(("x", Backend())), Frontend())(
      Return(Call("x", List()))
    ) should matchPattern {case Right(List(
      NetCall(_, "x", List()),
      Return(Name(_))
    )) => }
  }
  
  it should "handle functions in expressions" in {
    Flatten.flatten(Map(("x", Backend())), Frontend())(
      Return(Bop(Plus(), Call("x", List(Name("y"))), Name("y")))
    ) should matchPattern {case Right(List(
      NetCall(_, "x", List(Name("y"))),
      Return(Bop(Plus(), Name(_), Name("y"))
    ))) => }
  }
  
  it should "handle calls within function arguments" in {
    Flatten.flatten(Map(("x", Backend())), Frontend())(
      Return(Call("x", List(Call("x", List(Name("y"))))))
    ) should matchPattern {case Right(List(
      NetCall(a, "x", List(Name("y"))),
      NetCall(b, "x", List(Name(c))),
      Return(Name(d))
    )) => }
  }
}
