class Tests extends org.scalatest.FunSuite {

  import PathImplicits._
  import TimeImplicits._
  import java.nio.file._
  import java.time.LocalDate

  test("Write"){
  	val p = Paths.get("test4.txt")
  	p.append("Cofefe")
  	assert(p.read() == p.read())
  }
  test("Month Check"){
  	val jan1 = 1.jan
  	assert(jan1 == LocalDate.of(2018, 1, 1))
  }
  test("day Check"){
  	val jan1 = 1.jan + 1.year
  	assert(jan1.getDayOfMonth() == 1 && jan1.getYear() == 2019)
  }
}
