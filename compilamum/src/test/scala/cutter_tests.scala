import cutter.{Cut}
import ast._

import org.scalatest._

class CuttingSpec extends FlatSpec with Matchers {
  "Cutter" should "flatten basic function calls" in {
    Cut.flatten(Map(("x", Backend())), Frontend())(
      Return(Call(Name("x"), List()))
    ) should be (Right(List(
      NetCall("__cut_tmp_var__", Call(Name("x"), List())),
      Return(Name("__cut_tmp_var__"))
    )))
  }
}
