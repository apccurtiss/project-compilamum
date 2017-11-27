import parser.{Parseamum,ParseError}
import ast._

import collection.mutable.Stack
import org.scalatest._

class ParsingSpec extends FlatSpec with Matchers {
  "Parseamum" should "reduce constant expressions during parsing" in {
    Parseamum("1 + 1;") should be (Right(List(Discard(ConstFloat(2.0)))))
  }
}
