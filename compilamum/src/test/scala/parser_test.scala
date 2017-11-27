import parser.{Parseamum,ParseError}
import ast._

import collection.mutable.Stack
import org.scalatest._

class ParsingSpec extends FlatSpec with Matchers {
  "Parseamum" should "be able to parse discard expressions" in {
    Parseamum("1;") should be (Right(List(Discard(ConstFloat(1.0)))))
  }
  
  it should "be able to parse strings" in {
    Parseamum("\"Hello world\";") should be (Right(List(Discard(ConstString("Hello world")))))
  }
  
  it should "be able to parse booleans" in {
    Parseamum("True;") should be (Right(List(Discard(ConstBool(true)))))
  }
  
  it should "reduce constant expressions during parsing" in {
    Parseamum("1 + 1;") should be (Right(List(Discard(ConstFloat(2.0)))))
  }
  
  it should "be able to parse while loops" in {
    Parseamum("while (1) 1;") should be (Right(List(While(ConstFloat(1.0), Discard(ConstFloat(1.0))))))
  }
}
