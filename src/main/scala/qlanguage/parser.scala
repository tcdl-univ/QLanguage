package qlanguage

import scala.util.parsing.combinator._

case class ParseException(message: String) extends RuntimeException(message)

object QLanguageParser extends QLanguageParser
trait QLanguageParser extends RegexParsers {

  protected lazy val program = sentence.* ^^ Program
  protected lazy val sentence: Parser[BaseNode] = function | minus | choose | apply | numeral | alt_numeral | expression

  //expressions
  protected lazy val expression: Parser[Expression] = numericExpression | stringExpression
  protected lazy val numericExpression = "[0-9_]+".r ^^ { e => IntExpression(e.toInt) }
  protected lazy val stringExpression = "[a-zA-Z_]+".r ^^ StrExpression

  //Operations
  protected lazy val minus = "("~> expression ~ "-" ~ expression <~ ")" ^^ { case left ~ "-" ~ right => Minus(left, right) }
  protected lazy val choose = "choose(" ~> expression ~ "," ~ expression <~")" ^^ { case left ~ "," ~ right => Choose(left, right) }
  protected lazy val apply = "apply(" ~> function ~ "," ~ expression <~ ")" ^^ { case function ~ "," ~ expression => Apply(function, expression) }
  protected lazy val alt_numeral = "@" ~> sentence ^^ Query
  protected lazy val numeral = "numeral?" ~> sentence ^^ Query

  //lambda
  protected lazy val function = "lambda" ~> expression ~ '.' ~ expression.? ^^ { case x ~ '.' ~ e => Lambda(x, e) }

  def apply(input: String) = parseAll(program, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, _)  => throw ParseException(msg)
  }
}