import hw.tictactoe._

class Game(turn: Player, dim: Int, board: Map[(Int, Int), Player]) extends GameLike[Game] {
  def isFinished(): Boolean = {
  	xWin() || oWin() || filled()
  }
  def varWin(): Boolean = {
  	if(turn == X){
  		xWin()
  	}else if(turn == O){
  		oWin()
  	}else{
  		println("Error")
  		false
  	}
  }
  def otherWin(): Boolean = {
  	if(turn == X){
  		oWin()
  	}else if(turn == O){
  		xWin()
  	}else{
  		println("Error")
  		false
  	}
  }
  def getTurn(): Player = {
  	if(turn == X){
  		X
  	}else if(turn == O){
  		O
  	}else{
  		println("Error")
  		X
  	}
  }
  def otherTurn(): Player = {
  	if(turn == X){
  		O
  	}else if(turn == O){
  		X
  	}else{
  		println("Error")
  		X
  	}
  }
  def xWin(): Boolean = {
  	xAllVertHor(0, dim) || xWinDiaL(0, 0, dim) || xWinDiaR(dim-1, 0, dim)
  }
  def xAllVertHor(ind: Int, dim: Int): Boolean = {
  	if(ind > dim-1){
  		false
  	}else if(xWinVert(ind, 0, dim) || xWinHor(0, ind, dim)){
  		true
  	}else{
  		xAllVertHor(ind+1, dim)
  	}
  }
  def xWinVert(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(X)){
  		if(y == dim-1){
  			true
  		}else{
  			xWinVert(x, y+1, dim)
  		}
  	}else{
  		false
  	}
  }
  def xWinHor(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(X)){
  		if(x == dim-1){
  			true
  		}else{
  			xWinHor(x+1, y, dim)
  		}
  	}else{
  		false
  	}
  }
  def xWinDiaL(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(X)){
  		if(y == dim-1 && x == dim-1){
  			true
  		}else{
  			xWinDiaL(x+1, y+1, dim)
  		}
  	}else{
  		false
  	}
  }
  def xWinDiaR(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(X)){
  		if(y == dim-1 && x == 0){
  			true
  		}else{
  			xWinDiaR(x-1, y+1, dim)
  		}
  	}else{
  		false
  	}
  }

  def oWin(): Boolean = {
  	oAllVertHor(0, dim) || oWinDiaL(0, 0, dim) || oWinDiaR(dim-1, 0, dim)
  }
  def oAllVertHor(ind: Int, dim: Int): Boolean = {
  	if(ind > dim-1){
  		false
  	}else if(oWinVert(ind, 0, dim) || oWinHor(0, ind, dim)){
  		true
  	}else{
  		oAllVertHor(ind+1, dim)
  	}
  }
  def oWinVert(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(O)){
  		if(y == dim-1){
  			true
  		}else{
  			oWinVert(x, y+1, dim)
  		}
  	}else{
  		false
  	}
  }
  def oWinHor(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(O)){
  		if(x == dim-1){
  			true
  		}else{
  			oWinHor(x+1, y, dim)
  		}
  	}else{
  		false
  	}
  }
  def oWinDiaL(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(O)){
  		if(y == dim-1 && x == dim-1){
  			true
  		}else{
  			oWinDiaL(x+1, y+1, dim)
  		}
  	}else{
  		false
  	}
  }
  def oWinDiaR(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(O)){
  		if(y == dim-1 && x == 0){
  			true
  		}else{
  			oWinDiaR(x-1, y+1, dim)
  		}
  	}else{
  		false
  	}
  }
  def filled(): Boolean = {
  	fillAllVert(0, dim)
  }
  def fillAllVert(ind: Int, dim: Int): Boolean = {
  	if(ind > dim - 1){
  		true
  	}else if(!fillVert(ind, 0, dim)){
  		false
  	}else{
  		fillAllVert(ind+1, dim)
  	}
  }
  def fillVert(x: Int, y: Int, dim: Int): Boolean = {
  	if(board.get((x,y)) == Some(O) || board.get((x,y)) == Some(X)){
  		if(y == dim-1){
  			true
  		}else{
  			fillVert(x, y+1, dim)
  		}
  	}else{
  		false
  	}
  }
  /* Assume that isFinished is true */
  def getWinner(): Option[Player] = {
  	if(xWin()){
  		Some(X)
  	}else if(oWin()){
  		Some(O)
  	}else{
  		None
  	}
  }
  def nextBoards(): List[Game] = {
  	allVertBoards(0, dim)
  }
  def allVertBoards(ind: Int, dim: Int): List[Game] = {
  	if(ind == dim){
  		Nil
  	}else{
  		vertBoards(ind, 0, dim) ::: allVertBoards(ind+1, dim)
  	}
  }
  def vertBoards(x: Int, y: Int, dim: Int): List[Game] = {
  	if(y > dim - 1){
  		Nil
  	}else if(board.get((x,y)) == Some(O) || board.get((x,y)) == Some(X)){
  		vertBoards(x, y+1, dim)
  	}else{
  		new Game(otherTurn(), dim, (board + ((x, y) -> turn))) :: vertBoards(x, y+1, dim)
  	}
  }
}

object Solution extends MinimaxLike {
  type T = Game // T is an "abstract type member" of MinimaxLike
  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
  	new Game(turn, dim, board)
  }
  def minimax(board: Game): Option[Player] = {
  	if(board.isFinished){
  		if(board.varWin()){
  			println("ended on turn")
  			Some(board.getTurn())
  		}else if(board.otherWin()){
  			println("ended on not turn")
  			Some(board.otherTurn())
  		}else{
  			None
  		}
  	}else{
  		if(minimaxList(board.nextBoards, board.getTurn()) == Some(board.getTurn())){
  			Some(board.getTurn())
  		}else if(minimaxNoneCheck(board.nextBoards, board.getTurn()) == None){
  			None
  		}else{
  			println("defualted")
  			Some(board.otherTurn())
  		}
  	}
  }
  def minimaxList(boards: List[Game], check: Player): Option[Player] = boards match{
  	case Nil => None
  	case first :: rest => {
  		if(minimax(first) == Some(check)){
  			Some(check)
  		}else{
  			minimaxList(rest, check)
  		}
  	}
  }
  def minimaxNoneCheck(boards: List[Game], check: Player): Option[Player] = boards match{
  	case Nil => Some(X)
  	case first :: rest => {
  		if(minimax(first) == None){
  			None
  		}else{
  			minimaxNoneCheck(rest, check)
  		}
  	}
  }
}
