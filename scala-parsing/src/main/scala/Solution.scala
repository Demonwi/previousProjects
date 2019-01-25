import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
  def eval(e: Expr): Double = {
  	e match{
  		case Num(n) => n

  		case Add(n, m) => eval(n) + eval(m)

  		case Sub(n, m) => eval(n) - eval(m)

  		case Mul(n, m) => eval(n) * eval(m)

  		case Div(n, m) => eval(n) / eval(m)

  		case Exponent(n, m) => Math.pow(eval(n), eval(m))
  	}
  }
}

object ArithParser extends ArithParserLike {

  // number: PackratParser[Double] is defined in ArithParserLike

  // reverse order of operations expr -> add/sub -> mult/div -> exp -> parentatsis

  lazy val atom: PackratParser[Expr] = number ^^ { case n => Num(n)} | "(" ~>expr<~ ")"

  lazy val exponent: PackratParser[Expr] = exponent ~ "^" ~ atom ^^ { case n ~ _ ~ m => Exponent(n, m)} | atom

  lazy val add: PackratParser[Expr] = add ~ "+" ~ mul ^^ { case n ~ _ ~ m => Add(n, m)} | add ~ "-" ~ mul ^^ { case n ~ _ ~ m => Sub(n, m)} | mul

  lazy val mul: PackratParser[Expr] = mul ~ "*" ~ exponent ^^ { case n ~ _ ~ m => Mul(n, m)} | mul ~ "/" ~ exponent ^^ { case n ~ _ ~ m => Div(n, m)} | exponent

  lazy val expr: PackratParser[Expr] = add
}

object ArithPrinter extends ArithPrinterLike {
  def print(e: Expr): String = {
  	// same recursive logic as eval
  	e match{
  		case Num(n) => n.toString

		case Add(n, m) => print(n).toString + "+" + print(m).toString

		case Sub(n, m) => print(n).toString + "-" + print(m).toString

		case Mul(n, m) => print(n).toString + "*" + print(m).toString

		case Div(n, m) => print(n).toString + "/" + print(m).toString

		case Exponent(n, m) => print(n).toString + "^" + print(m).toString
  	}
  }
}