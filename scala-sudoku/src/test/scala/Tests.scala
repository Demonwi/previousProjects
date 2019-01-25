
class TestSuite extends org.scalatest.FunSuite {
	import hw.sudoku._
	import Solution._

  test("The solution object must be defined") {
    val obj : hw.sudoku.SudokuLike = Solution
  }

 

  test("valAT"){
  	val str = "1................................................................................"
  	val strList = str.toList
  	val newMap = parse(str)
  	assert(newMap.valueAt(0,1) == None)
  }

  test("peers"){
  	assert(peers(0, 7).length == 20)
  }
 
  /*test("nextBoards"){
  	val str = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
  	val aBoard = parse(str)
  	val bBoards = aBoard.nextStates()
  	for(b <- bBoards){
  		b.printBoard(0)
  		println()
  	}
  	assert(true)
  }*/
  // "639574182541829376782613954198467523365982417427135869956748231813296745274351698"

  test("place"){
  	
  	val aBoard = emptyBoard.place(0,0,1).place(0, 1, 2).place(0, 2, 3).place(0, 3, 4).place(0, 4, 5).place(0, 5, 6).place(0, 6, 7).place(0, 7, 8)
  	assert(aBoard.availableValuesAt(1, 8) == List(1,2,3,4,5,6))
  }
  //val str = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
  test("Solve"){
  	val str = ".....3.514...2.7...6........97.4.......1..4...5.....39..9.7...66......4.3...1...2"
  	val aBoard = parse(str)
  	val sBoard = aBoard.solve()
  	sBoard match{
  		case None => println("FailWhale")
  		case Some(x) => x.printBoard(0)
  	}
  	assert(aBoard.availableValuesAt(2, 6) == List())
  }
}