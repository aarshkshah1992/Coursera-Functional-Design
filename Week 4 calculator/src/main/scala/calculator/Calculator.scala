package calculator

import scala.concurrent.Future
import scala.concurrent.duration.DurationConversions

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, sig) => (name, Signal(eval(name, sig(), namedExpressions)))
    }
  }

  def eval(cell: String, expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    Future{1}.recover()
    val a = 5 DurationConversions

    expr match {

      case Literal(v) => v
      case Plus(a, b) => eval(cell, a, references) + eval(cell, b, references)
      case Minus(a, b) => eval(cell, a, references) - eval(cell, b, references)
      case Times(a, b) => eval(cell, a, references) * eval(cell, b, references)
      case Divide(a, b) => eval(cell, a, references) / eval(cell, b, references)
      case Ref(name) =>
        if (cell == name) Double.NaN
        else {
          val expr = references.get(name)
          expr.map(sig => eval(cell, sig(), references)).getOrElse(Double.NaN)
        }
    }


  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
