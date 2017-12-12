import parser.{Parse,ParseError}
import ast._

import org.scalatest._

class ParsingSpec extends FlatSpec with Matchers {
  "Parse" should "parse discard expressions" in {
    Parse.parseBlock("1;") should be (Right(List(Discard(ConstFloat(1.0)))))
  }

  it should "parse strings" in {
    Parse.parseBlock("\"Hello world\";") should be (Right(List(Discard(ConstString("Hello world")))))
  }

  it should "parse booleans" in {
    Parse.parseBlock("True;") should be (Right(List(Discard(ConstBool(true)))))
  }

  it should "reduce constant expressions during parsing" in {
    Parse.parseBlock("1 + 1;") should be (Right(List(Discard(ConstFloat(2.0)))))
    Parse.parseBlock("1 + 1 + 1;") should be (Right(List(Discard(ConstFloat(3.0)))))
    Parse.parseBlock("1 + 1 + 1 + 1;") should be (Right(List(Discard(ConstFloat(4.0)))))
  }

  it should "parse while loops" in {
    Parse.parseBlock("while (1) 1;") should be (Right(List(While(ConstFloat(1.0), Discard(ConstFloat(1.0))))))
  }

  it should "parse break statements" in {
    Parse.parseBlock("while (1) break;") should be (Right(List(While(ConstFloat(1.0), Break()))))
    Parse.parseBlock("break;") should be (Right(List(Break())))
  }

  it should "parse if statements" in {
    Parse.parseBlock("if (1) 1; else 2;") should be (Right(List(
      If(ConstFloat(1.0),Discard(ConstFloat(1.0)),Discard(ConstFloat(2.0)))
    )))
  }

  it should "parse variable declarations and assignments" in {
    Parse.parseBlock("let x: Number = 0; x = 45;") should be (Right(List(
      Declare("x", Num(), ConstFloat(0.0)),
      Assign("x", ConstFloat(45.0))
    )))
  }

  it should "parse return values" in {
    Parse.parseBlock("return True;") should be (Right(List(Return(ConstBool(true)))))
  }

  it should "parse functions" in {
    Parse("frontend one(): Number { return 1; }") should be (
      Right(Program(List(GlobalFuncDecl(Frontend(),Num(),"one",List(),Block(List(Return(ConstFloat(1.0))))))))
    )
  }

  it should "parse global declarations" in {
    Parse("frontend x: Number = 1;") should be (
      Right(Program(List(GlobalDecl(Frontend(),"x",Num(),ConstFloat(1.0)))))
    )
  }

  it should "parse function call" in {
    Parse("frontend bottom(): Number { return bottom(); }") should be (
      Right(Program(List(GlobalFuncDecl(Frontend(),Num(),"bottom",List(),Block(List(Return(Call("bottom", List()))))))))
    )
  }

  it should "parse parameters in functions correctly" in {
    Parse("frontend bottom(i: Number, s: String): Number { return bottom(i, s); }") should be (
      Right(Program(List(GlobalFuncDecl(
        Frontend(),Num(),"bottom",List(("i", Num()), ("s", Str())),
        Block(List(Return(Call("bottom", List(Name("i"), Name("s"))))))
      ))))
    )
  }
}
