class Tests extends org.scalatest.FunSuite {

  import Main._

  test("Implemented the Solution interface") {
    val main: hw.streams.SolutionLike = Main
  }

  test("From test") {
  	assert(nth[Int](from(5), 5) == 10)
  }

  test("Map test") {
  	assert(nth[Int](map[Int,Int](x => 2*x, from(0)), 5) == 10)
  }

  // 1 3 5 7
  test("Filter + sift test"){
  	assert(nth[Int](sift(2, from(0)), 5) == 11)
  }

  test("Interleave test"){
  	assert(nth[Int](interleave(from(0), from(0)), 8) == 4)
  }

  test("Total test"){
  	assert(nth[Double](total(fromDoub(0)), 4) == 10.0)
  }

  test("Prime test"){
  	assert(nth[Int](prime, 4) == 11)
  }
}
