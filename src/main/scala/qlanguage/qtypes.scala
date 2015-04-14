package qlanguage
import scala.language.implicitConversions
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

case class Message(pos: Position, msg: String)

object BooleanResult {
  class MyBooleanLogic[T](x: Option[T], origin: T) {
    def |||(defaultValue: T) = x.getOrElse(defaultValue)
    def &&&(defaultValue: T) = x.isDefined match {
      case true  => defaultValue
      case false => origin
    }

  }

  implicit def toOption(res: Result) = {
    new MyBooleanLogic(Option(res).filter(_ != Undefined()), res)
  }
}
import BooleanResult._

//Invalid State
abstract class Result {
  val value: Int

  def union(r: Result): Result
}

case class ValidResult(val value: Int) extends Result {
  def union(r: Result): Result = r match {
    case ValidResult(_) => this
    case Undefined(_)   => Undefined()
    case _              => Undefined()
  }
}
case class Undefined(val value: Int = -1) extends Result {
  def union(r: Result): Result = this
}

//Node hierarchy
sealed trait BaseNode extends Positional{
  def evaluate(): Result
}
sealed trait Expression extends BaseNode
sealed trait Operation extends BaseNode

//Concrete Nodes
case class Program(expressions: List[BaseNode])
case class Apply(function: Lambda, expression: BaseNode) extends Operation {
  def evaluate(): Result = {
    return new Lambda(function.x, expression).evaluate()
  }
}
case class Choose(left: Expression, right: Expression) extends Operation {
  def evaluate(): Result = {
    val eleft: Result = left.evaluate()
    val eright: Result = right.evaluate()

    eleft union eright

  }
}
case class Query(expression: BaseNode) extends Operation {
  def evaluate(): Result = {
    val value = expression match {
      case IntExpression(1) => 1
      case IntExpression(0) => 1
      case _                => 0
    }
    return new ValidResult(value)
  }

}

case class Minus(left: Expression, right: Expression) extends Operation {
  def evaluate(): Result = {
    val eleft: Result = left.evaluate()
    val eright: Result = right.evaluate()

    if (eleft == Undefined(-1) || eright == Undefined(-1) || eleft.value < eright.value)
      return new Undefined
    return new ValidResult(eleft.value - eright.value)
  }
}

case class StrExpression(value: String) extends Expression {
  def evaluate: Result = new Undefined
}

object VarExpression {
  def apply(name: String): VarExpression = VarExpression(name, Scope.TOP)
}
case class VarExpression(name: String, scope: Scope) extends Expression {
  def evaluate(): Result = new ValidResult(0)
}

case class IntExpression(value: Int) extends Expression {
  def evaluate(): Result = value match {
    case 0 => new ValidResult(0)
    case 1 => new ValidResult(1)
    case _ => new Undefined
  }

}
case class Lambda(x: VarExpression, e: BaseNode) extends BaseNode {
  def evaluate(): Result = {
    val evaluated = e.evaluate()
    if (evaluated == Undefined())
      return Undefined()
    evaluated union (evaluated ||| Undefined())
  }
}