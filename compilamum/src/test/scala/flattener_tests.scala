import flattener.{Flatten}
import ast._

import org.scalatest._

class CuttingSpec extends FlatSpec with Matchers {
  "Cutter" should "flatten basic function calls" in {
    Flatten.flatten(
      Return(Call("x", List()))
      , Set()
    ) should matchPattern {case Right(List(
      CallStmt(_, "x", List(), _, _),
      Return(Name(_))
    )) => }
  }
  
  it should "handle functions in expressions" in {
    Flatten.flatten(
      Return(Bop(Plus(), Call("x", List(Name("y"))), Name("y")))
      , Set()
    ) should matchPattern {case Right(List(
      CallStmt(_, "x", List(Name("y")), _, _),
      Return(Bop(Plus(), Name(_), Name("y"))
    ))) => }
  }
  
  it should "handle calls within function arguments" in {
    Flatten.flatten(
      Return(Call("x", List(Call("x", List(Name("y"))))))
      , Set()
    ) should matchPattern {case Right(List(
      CallStmt(a, "x", List(Name("y")), _, _),
      CallStmt(b, "x", List(Name(c)), _, _),
      Return(Name(d))
    )) => }
  }
}
