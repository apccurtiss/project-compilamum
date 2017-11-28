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
    Parseamum("1 + 11;") should be (Right(List(Discard(ConstFloat(12.0)))))
  }
  
  it should "be able to parse while loops" in {
    Parseamum("while (1) 1;") should be (Right(List(While(ConstFloat(1.0), Discard(ConstFloat(1.0))))))
  }
  
  it should "be able to parse break statements" in {
    Parseamum("while (1) break;") should be (Right(List(While(ConstFloat(1.0), Break()))))
  }
  
  it should "be able to parse if statements" in {
    Parseamum("if (1) 1; else 2;") should be (Right(List(
      If(ConstFloat(1.0),Discard(ConstFloat(1.0)),
      Discard(ConstFloat(2.0)))
    )))
  }
  
  it should "be able to parse variable declarations and assignments" in {
    Parseamum("let x: Number = 0; x = 45;") should be (Right(List(
      Declare("x", Num(), ConstFloat(0.0)),
      Assign("x", ConstFloat(45.0))
    )))
  }
  
  it should "be able to parse return values" in {
    Parseamum("return True;") should be (Right(List(Return(ConstBool(true)))))
  }
}
