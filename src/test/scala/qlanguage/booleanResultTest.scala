package qlanguage

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import BooleanResult._

class QBooleanResultsTest extends FreeSpec with Matchers {

  "QLanguage evaluation of" - {

    "Int Expressions" - {
      "valid and boolean" in {
        (ValidResult(1) &&& ValidResult(0)).equals(false)
      }
      
      "invalid and boolean" in {
        (Undefined() &&& ValidResult(0)).equals(false)
      }
      
      "valid or boolean" in {
        (Undefined() ||| ValidResult(0)).equals(true)
      }
      
      "invalid or boolean" in {
        (Undefined() ||| Undefined()).equals(true)
      }
    }
  }
}