import hw.sudoku._

object Solution extends SudokuLike {
  
  type T = Board

  def calcAllPos(ix: Int): List[(Int, Int)] = {
    if (ix == 81) Nil else (ix / 9, ix % 9) :: calcAllPos(ix + 1)
  }

  val allPos = calcAllPos(0)

  val oneTo9 = 1.to(9).toList

  val emptyBoard = new Board(allPos.map(coord => coord -> oneTo9).toMap)

  def parseHelper(alist: List[(Char, (Int, Int))]): Board = alist match {
    case Nil => emptyBoard
    case ('.', _) :: rest => parseHelper(rest)
    case (digit, (row, col)) :: rest => {
      val n = digit.toString.toInt
      parseHelper(rest).place(row, col, n)
    }
  }

  def parse(str: String): Board = parseHelper(str.toList.zip(allPos))

  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map(r => (r,col))
    val colPeers = 0.to(8).map(c => (row, c))
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    val boxPeers = boxRow.to(boxRow + 2).flatMap(r =>
      boxCol.to(boxCol + 2).map(c => (r, c)))
    // Remove duplicates and (row, col)
    (rowPeers ++ colPeers ++ boxPeers).toSet.diff(Set((row, col))).toList
  }

  val peersTbl = allPos.map(pos => {
    val (row, col) = pos
    pos -> calcPeers(row, col)
  }).toMap

  def peers(row: Int, col: Int): List[(Int, Int)] = peersTbl((row, col))

}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
//    available.getOrElse((row, col), 1.to(9).toList)
    available(row, col)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    if(available(row, col).length == 1){
      Some(available(row, col).head)
    }else{
      None
    }
  }

  def isSolved(): Boolean = {
    filled(0) && !(isUnsolvable())
  }

  def filled(current: Int): Boolean = {
    if(current > 80){
      true
    }else{
      if(available(current % 9, current/9).length == 1){
        filled(current+1)
      }else{
        false
      }
    }
  }

  def fillCount(current: Int): Int = {
    if(current > 80){
      0
    }else{
      if(available(current % 9, current/9).length == 1){
        1 + fillCount(current + 1)
      }else{
        fillCount(current + 1)
      }
    }
  }
// Contradictions or has blocks with no possibilities
  def isUnsolvable(): Boolean = {
    if(hasEmpty(0)){
      true
    }else{
      false
    }
  }

  def hasEmpty(current: Int): Boolean = {
    if(current > 80){
      false
    }else{
      if(available(current % 9, current/9).length == 0){
        true
      }else{
        hasEmpty(current + 1)
      }
    }
  }
  


  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))

    if(valListMaker(Solution.peers(row, col)).contains(value)){
      new Board(available + ((0,0) -> List()))
    }else{

      val placedBoard = new Board(available + ((row, col) -> List(value)))
      placedBoard.numRemoval(Solution.peers(row, col).filter(n => availableValuesAt(n._1, n._2).length > 1), value)
    }
  }

  def valListMaker(coords: List[(Int, Int)]): List[Int] = coords match{
    case head :: tail => {
      if(available(head._1, head._2).length == 1){
        available(head._1, head._2).head :: valListMaker(tail)
      }else{
        valListMaker(tail)
      }
    }
    case _ => Nil
  }

  def numRemoval(peerList: List[(Int, Int)], num: Int): Board = peerList match{
    case head :: tail => {
      val (row, col) = head
      val curList = available(row, col)
//      println("From " + row + ", " + col)
      val newList= listRemove(curList, num)
      val newBoard = new Board(available + ((row, col) -> newList))
      if(newList.length == 1 && curList.length != 1){
        val placedBoard = newBoard.numRemoval(tail, num).place(row, col, newList.head)
        if(placedBoard.availableValuesAt(row, col).contains(newList.head)){
          placedBoard
        }else{
          val newerBoard = new Board(available + ((row, col) -> List()))
          newerBoard.numRemoval(tail, num)
        }
      }else{
        newBoard.numRemoval(tail, num)
      }
    }
    case Nil => new Board(available)
    case _ => new Board(available)
  }

  

  def listRemove(aList: List[Int], rem: Int): List[Int] = {
//    println("Removed Value = " + rem)
    aList.filter(n => n != rem)
  }

  def allLT(num: Int, aList: List[Board]): List[Board] = {
    aList match{
      case Nil => Nil
      case n :: tail => {
        if(aLevel(n, 0) < num){
          n :: allLT(num, tail)
        }else{
          allLT(num, tail)
        }
      }
    }
  }

  def allGTE(num: Int, aList: List[Board]): List[Board] = {
    aList match{
      case Nil => Nil
      case n :: tail => {
        if(num <= aLevel(n, 0)){
          n :: allGTE(num, tail)
        }else{
          allGTE(num, tail)
        }
      }
    }
  }

  def quickSort(aList: List[Board]): List[Board] = {
    aList match{
      case Nil => Nil
      case n :: Nil => n :: Nil
      case n :: tail => {
        quickSort(allLT(aLevel(n, 0), tail)) ::: n :: Nil ::: quickSort(allGTE(aLevel(n, 0), tail))
      }
    }
  }

  def aLevel(aBoard: Board, current: Int): Int = {
    if(current > 80){
      0
    }else{
      aBoard.availableValuesAt(current%9, current/9).length + aLevel(aBoard, current + 1)
    }
  }


  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if (isUnsolvable()) {
      List()
    }else {
      quickSort(allBoards(0)/*.filter(_.fillCount(0) > fillCount(0))*/)
    }
  }

  def allBoards(current: Int): List[Board] = {
    if(current > 80){
      Nil
    }else if(available(current%9, current/9).length > 1){
      available(current%9, current/9).map(n => place(current%9, current/9, n))  ::: allBoards(current + 1)
    }else{
      allBoards(current + 1)
    }
  }

  def printBoard(current: Int): Boolean = {
    if(current > 80){
      println()
      true
    }else{
      if(current%9 == 0){
        println()
      }
      if(availableValuesAt(current%9, current/9).length == 1){
        print(availableValuesAt(current%9, current/9).head + " ")
      }else if(availableValuesAt(current%9, current/9).length == 0){
        print(0 + " ")
      }else{
        print(". ")
      }
      printBoard(current + 1)
    }
  }

  def solve(): Option[Board] = {
    println("filled" + fillCount(0))
    println(availableValuesAt(2,7).length)
    printBoard(0)
    if(isSolved()){
//      println("Finished *****************************************************************************")
      Some(this)
    }else if(isUnsolvable()){
      printBoard(0)
      None
    }else{
      solveList(nextStates())
    }
  }

  def solveList(nextBoards: List[Board]): Option[Board] = nextBoards match{
    case head :: tail => {
      println("Boards: " + nextBoards.length)
      val hSolve = head.solve()
      if(hSolve == None){
        solveList(tail)
      }else{
        hSolve
      }
    }
    case _ => None
  }
}