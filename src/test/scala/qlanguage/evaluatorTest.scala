package qlanguage

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class QEvaluatorTest extends FreeSpec with Matchers {

  "QLanguage evaluation of" - {

    "Int Expressions" - {
      "invalid" in {
        IntExpression(3).evaluate() should be(Undefined(-1))
        IntExpression(3424).evaluate() should be(Undefined(-1))
      }

      "0" in {
        IntExpression(0).evaluate() should be(ValidResult(0))
      }

      "1" in {
        IntExpression(1).evaluate() should be(ValidResult(1))
      }
    }
    
    "Str Expressions" - {
      "Invalid" in {
        StrExpression("Hello").evaluate() should be(Undefined(-1))
      }
    }

    "Minus" - {
      "Simple" in {
        Minus(IntExpression(1), IntExpression(0)).evaluate() should be(ValidResult(1))
      }
      
      "0-1 should be invalid" in {
        Minus(IntExpression(0), IntExpression(1)).evaluate() should be(Undefined(-1))
      }
      
      "134 - 1 should be invalid" in {
        Minus(IntExpression(134), IntExpression(1)).evaluate() should be(Undefined(-1))
      }
      
      "Hello - 1 should be invalid" in {
        Minus(StrExpression("Hello"), IntExpression(1)).evaluate() should be(Undefined(-1))
      }
      
      "Other Simple" in {
        Minus(IntExpression(0), IntExpression(0)).evaluate() should be(ValidResult(0))
        Minus(IntExpression(1), IntExpression(1)).evaluate() should be(ValidResult(0))
      }
    }
    
    "Query of Minus" - {
      "1 expr only should be 1 " in {
        Query(IntExpression(1)).evaluate() should be(ValidResult(1))
      }
      
      "0 expr only should be 1 " in {
        Query(IntExpression(0)).evaluate() should be(ValidResult(1))
      }
      
      "Should be 0" in {
        Query(Minus(IntExpression(1),IntExpression(0))).evaluate() should be(ValidResult(0)) 
      }
      
      "Invalid should be 0 too" in {
        Query(Minus(IntExpression(3),IntExpression(1))).evaluate() should be(ValidResult(0)) 
      }
    }
  }

}
