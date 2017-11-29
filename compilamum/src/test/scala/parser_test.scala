import parser.{Parseamum,ParseError}
import ast._

import collection.mutable.Stack
import org.scalatest._

class ParsingSpec extends FlatSpec with Matchers {
  "Parseamum" should "parse discard expressions" in {
    Parseamum.parseBlock("1;") should be (Right(List(Discard(ConstFloat(1.0)))))
  }
  
  it should "parse strings" in {
    Parseamum.parseBlock("\"Hello world\";") should be (Right(List(Discard(ConstString("Hello world")))))
  }
  
  it should "parse booleans" in {
    Parseamum.parseBlock("True;") should be (Right(List(Discard(ConstBool(true)))))
  }
  
  it should "reduce constant expressions during parsing" in {
    Parseamum.parseBlock("1 + 1;") should be (Right(List(Discard(ConstFloat(2.0)))))
    Parseamum.parseBlock("1 + 11;") should be (Right(List(Discard(ConstFloat(12.0)))))
  }
  
  it should "parse while loops" in {
    Parseamum.parseBlock("while (1) 1;") should be (Right(List(While(ConstFloat(1.0), Discard(ConstFloat(1.0))))))
  }
  
  it should "parse break statements" in {
    Parseamum.parseBlock("while (1) break;") should be (Right(List(While(ConstFloat(1.0), Break()))))
    Parseamum.parseBlock("break ;") should be (Right(List(Break())))
  }
  
  it should "parse if statements" in {
    Parseamum.parseBlock("if (1) 1; else 2;") should be (Right(List(
      If(ConstFloat(1.0),Discard(ConstFloat(1.0)),Discard(ConstFloat(2.0)))
    )))
  }
  
  it should "parse variable declarations and assignments" in {
    Parseamum.parseBlock("let x: Number = 0; x = 45;") should be (Right(List(
      Declare("x", Num(), ConstFloat(0.0)),
      Assign("x", ConstFloat(45.0))
    )))
  }
  
  it should "parse return values" in {
    Parseamum.parseBlock("return True;") should be (Right(List(Return(ConstBool(true)))))
  }
  
  it should "parse functions" in {
    Parseamum("frontend one() -> Number { return 1; }") should be (
      Right(List(FuncExpr(Frontend(),Num(),"one",Map(),Stmts(List(Return(ConstFloat(1.0)))))))
    )
  }
  
  it should "parse function call" in {
    Parseamum("frontend bottom() -> Number { return bottom(); }") should be (
      Right(List(FuncExpr(Frontend(),Num(),"bottom",Map(),Stmts(List(Return(Call(Name("bottom"), List())))))))
    )
  }
}
