package qlanguage

import scala.collection.mutable.ListBuffer

object Scope {
  var id = 0
  def nextId = { val i = id; id += 1; i }
  val TOP = new Scope(None, Set())
}

class Scope(val parent: Option[Scope], val boundNames: Set[String]) {

  val id = Scope.nextId

  def closestBinding(name: String): Option[Scope] =
    if (boundNames contains name)
      Some(this)
    else
      parent flatMap (_ closestBinding name)

}

class Binder(val defs: Map[String, BaseNode]) {
  val messages = ListBuffer[Message]()

  def apply(term: BaseNode) = bind(term, Scope.TOP)

  def bind(term: BaseNode, parent: Scope): BaseNode = term match {
    case Lambda(arg, body) =>
      val scope = new Scope(Some(parent), Set(arg.name))
      Lambda(arg.copy(scope = scope), bind(body, scope))
    case v @ VarExpression(name, _) if (defs contains name) =>
      bind(defs(name), parent)
    case v @ VarExpression(name, _) =>
      (parent closestBinding name) match {
        case Some(scope) =>
          v.copy(scope = scope)
        case None if name(0).isUpper =>
          v.copy(scope = Scope.TOP)
        case None =>
          messages += Message(v.pos, "Unbound variable: " + name)
          v
      }
    case Apply(fun, arg) =>
      Apply(bind(fun, parent).asInstanceOf[Lambda], bind(arg, parent))
  }

}