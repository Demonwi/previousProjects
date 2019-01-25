import ArithEval._
import ArithParser._
import ArithPrinter._
import hw.parsing._

class TrivialTestSuite extends org.scalatest.FunSuite {

	test("several objects must be defined") {
	  	val parser: hw.parsing.ArithParserLike = ArithParser
	 	val printer: hw.parsing.ArithPrinterLike = ArithPrinter
	  	val eval: hw.parsing.ArithEvalLike = ArithEval
	}

	test("addition"){
		assert(eval(parseArith("3 + 4")) == 7)
	}
	test("mult"){
		assert(eval(parseArith("12 * 12")) == 144)
	}
	test("mult add"){
		assert(eval(parseArith("5 * 6 + -4 * 3")) == 18)
	}
	test("exponents added"){
		assert(eval(parseArith("7 + 3 * 2 ^ 2")) == 19)
	}
	test("parentasis test"){
		assert(eval(parseArith("((2 + 3) * 4) ^ 0 + 1")) == 2)
	}

}