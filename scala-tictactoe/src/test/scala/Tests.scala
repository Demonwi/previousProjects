class Tests extends org.scalatest.FunSuite {

  import Solution._
  import hw.tictactoe._

  test("xWin Verts 3x3"){
  	val testMapA = Map((0,0) -> X, (0,1) -> X, (0,2) -> X)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.xWin() == true)
  	val testMapB = Map((1,0) -> X, (1,1) -> X, (1,2) -> X)
  	val testGameB = new Game(X, 3, testMapB)
  	assert(testGameB.xWin() == true)
  	val testMapC = Map((2,0) -> X, (2,1) -> X, (2,2) -> X)
  	val testGameC = new Game(X, 3, testMapC)
  	assert(testGameC.xWin() == true)
  }
  test("xWin Hors 3x3"){
  	val testMapA = Map((0,0) -> X, (1,0) -> X, (2,0) -> X)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.xWin() == true)
  	val testMapB = Map((0,1) -> X, (1,1) -> X, (2,1) -> X)
  	val testGameB = new Game(X, 3, testMapB)
  	assert(testGameB.xWin() == true)
  	val testMapC = Map((2,0) -> X, (2,1) -> X, (2,2) -> X)
  	val testGameC = new Game(X, 3, testMapC)
  	assert(testGameC.xWin() == true)
  }
  test("xWin Dia 3x3"){
  	val testMapA = Map((0,0) -> X, (1,1) -> X, (2,2) -> X)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.xWin() == true)
  	val testMapB = Map((2,0) -> X, (1,1) -> X, (0,2) -> X)
  	val testGameB = new Game(X, 3, testMapB)
  	assert(testGameB.xWin() == true)
  }

  test("oWin Verts 3x3"){
  	val testMapA = Map((0,0) -> O, (0,1) -> O, (0,2) -> O)
  	val testGameA = new Game(O, 3, testMapA)
  	assert(testGameA.oWin() == true)
  	val testMapB = Map((1,0) -> O, (1,1) -> O, (1,2) -> O)
  	val testGameB = new Game(O, 3, testMapB)
  	assert(testGameB.oWin() == true)
  	val testMapC = Map((2,0) -> O, (2,1) -> O, (2,2) -> O)
  	val testGameC = new Game(O, 3, testMapC)
  	assert(testGameC.isFinished() == true && testGameC.getWinner() == Some(O))
  }
  test("oWin Hors 3x3"){
  	val testMapA = Map((0,0) -> O, (1,0) -> O, (2,0) -> O)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.oWin() == true)
  	val testMapB = Map((0,1) -> O, (1,1) -> O, (2,1) -> O)
  	val testGameB = new Game(X, 3, testMapB)
  	assert(testGameB.oWin() == true)
  	val testMapC = Map((2,0) -> O, (2,1) -> O, (2,2) -> O)
  	val testGameC = new Game(X, 3, testMapC)
  	assert(testGameC.oWin() == true)
  }
  test("oWin Dia 3x3"){
  	val testMapA = Map((0,0) -> O, (1,1) -> O, (2,2) -> O)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.oWin() == true)
  	val testMapB = Map((2,0) -> O, (1,1) -> O, (0,2) -> O)
  	val testGameB = new Game(X, 3, testMapB)
  	assert(testGameB.oWin() == true)
  }
  test("filled 3x3"){
  	val testMapA = Map((0,0) -> O, (0,1) -> O, (0,2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> O, (2,0) -> O, (2,1) -> O, (2,2) -> O)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.filled() == true)
  }
  test("filled 3x3 Fail"){
  	val testMapA = Map((0,1) -> O, (0,2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> O, (2,0) -> O, (2,1) -> O, (2,2) -> O)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.filled() == false)
  }
  test("filled 3x3 Fail 2"){
  	val testMapA = Map((0,0) -> O, (0,1) -> O, (0,2) -> O, (1,0) -> O, (1,2) -> O, (2,0) -> O, (2,1) -> O, (2,2) -> O)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.filled() == false)
  }
  test("filled 3x3 Fail 3"){
  	val testMapA = Map((0,0) -> O, (0,1) -> O, (0,2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> O, (2,0) -> O, (2,1) -> O)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.filled() == false)
  }
  test("xWin 3x3 Fail"){
  	val testMapA = Map((0,0) -> X, (0,1) -> O, (0,2) -> X)
  	val testGameA = new Game(X, 3, testMapA)
  	assert(testGameA.xWin() == false)
  	val testMapB = Map((1,0) -> O, (1,1) -> X, (1,2) -> X)
  	val testGameB = new Game(X, 3, testMapB)
  	assert(testGameB.xWin() == false)
  	val testMapC = Map((2,0) -> X, (2,1) -> X, (2,2) -> O)
  	val testGameC = new Game(X, 3, testMapC)
  	assert(testGameC.xWin() == false)
  	val testMapD = Map((0,0) -> X, (1,1) -> O, (2,2) -> X)
  	val testGameD = new Game(X, 3, testMapD)
  	assert(testGameD.xWin() == false)
  }
  test("nextBoards 3x3"){
  	val testMapD = Map((0,0) -> O, (0,1) -> O, (0,2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> O, (2,0) -> O, (2,1) -> O, (2,2) -> O)
  	val testGameD = Solution.createGame(X, 3, testMapD)
  	assert(testGameD.nextBoards() == Nil)
  }
  test("minimax 3x3"){
  	val testMapA = Map((0,0) -> O, (0,1) -> X, (0,2) -> O, (1,0) -> X, (1,1) -> O, (1,2) -> X)
  	val testGameA = Solution.createGame(X, 3, testMapA)
  	assert(Solution.minimax(testGameA) == Some(O))
  }
}
