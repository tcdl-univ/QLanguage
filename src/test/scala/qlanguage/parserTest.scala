package qlanguage

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class QParserTest extends FreeSpec with Matchers with QLanguageParser {

  "QLanguage parse of" - {

    "should succeed" - {
      "for an empty program" in {
        apply(" ") should be(Program(List()))
      }

      "for a simple program" in {
        apply(" 1") should be(Program(List(IntExpression(1))))
      }

      "for a simple operation" in {
        apply(" numeral?1") should be(Program(List(Query(IntExpression(1)))))
      }

      "for a simple alternative syntax operation" in {
        apply(" @1") should be(Program(List(Query(IntExpression(1)))))
      }

      "for a composite operation" in {
        apply(" @(1-0)") should be(Program(List(Query(Minus(IntExpression(1), IntExpression(0))))))
      }
      
      "for a multiple line operation" in {
        apply(""" @(1-0) choose(2,5)""") should be(Program(List(Query(Minus(IntExpression(1),IntExpression(0))), Choose(IntExpression(2),IntExpression(5)))))
      }
      
      "Lambda parsing" in {
        apply(""" lambda x.(x - x)(0-1)""") should be(Program(List(Lambda(VarExpression("x"),Minus(StrExpression("x"),StrExpression("x"))), Minus(IntExpression(0),IntExpression(1)))))
      }
    }

  }

}
