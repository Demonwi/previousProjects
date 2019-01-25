class TrivialTestSuite extends org.scalatest.FunSuite {

  test("The Regexes object must be defined") {
    val regexes: hw.regex.RegexLike = Regexes
  }

  test("240 test shit"){
  	assert(iter(0, 0, 0, 0) == 0)
  }

}
