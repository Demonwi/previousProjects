object Lists {

  val oddNumbers = 1 :: 3 :: 5 :: Nil

  def sumDouble(aList: List[Int]): Int = {
    aList match {
      case Nil => 0
      case n :: tail => 2*n + sumDouble(tail)
    }
  }

  def removeZeros(aList: List[Int]): List[Int] = {
    aList match {
      case Nil => Nil
      case 0 :: tail => removeZeros(tail)
      case n :: tail => n :: removeZeros(tail)
    }
  }

  def countEvens(aList: List[Int]): Int = {
    aList match {
      case Nil => 0
      case n :: tail => {
        if(n%2 == 0){
          1 + countEvens(tail)
        }else{
          countEvens(tail)
        }
      }
    }
  }

  def removeAlternating(aList: List[Int]): List[Int] = {
    aList match {
      case Nil => Nil
      case n :: Nil => n :: Nil
      case a :: b :: tail => a :: removeAlternating(tail)
    }
  }

  def firstVal(aList: List[Int]): Int = {
    aList match {
      case Nil => throw new IllegalArgumentException("List must not be empty")
      case n :: tail => n
    }
  }

  def isAscending(aList: List[Int]): Boolean = {
    aList match {
      case Nil => true
      case n :: Nil => true
      case c :: d :: Nil => {
        if(c<=d){
          true
        }else{
          false
        }
      }
      case a :: b :: tail => {
        if((a<=b)&&(b<=firstVal(tail))){
          isAscending(tail)
        }else{
          false
        }
      }
    }
  }

  def addSub(aList: List[Int]): Int = {
    aList match {
      case Nil => 0
      case n :: Nil => n
      case a :: b :: tail => a - b + addSub(tail)
    }
  }

  def alternate(aList: List[Int], bList: List[Int]): List[Int] = {
    aList match {
      case Nil => Nil
      case a :: aTail => {
        bList match{
          case Nil => Nil
          case b :: bTail => {
            a :: b :: alternate(aTail, bTail)
          }
          case default => throw new IllegalArgumentException("Lists are not the same length")
        }
      }
    }
  }

  def fromTo(start: Int, finish: Int): List[Int] = {
    if(start >= finish){
      Nil
    }else{
      start :: fromTo(start+1, finish)
    }
  }

  def insertOrdered(num: Int, aList: List[Int]): List[Int] = {
    aList match{
      case Nil => num :: Nil
      case n :: tail => {
        if(num < n){
          num :: n :: tail
        }else{
          n :: insertOrdered(num, tail)
        }
      }
    }
  }

  def allLT(num: Int, aList: List[Int]): List[Int] = {
    aList match{
      case Nil => Nil
      case n :: tail => {
        if(n < num){
          n :: allLT(num, tail)
        }else{
          allLT(num, tail)
        }
      }
    }
  }

  def allGTE(num: Int, aList: List[Int]): List[Int] = {
    aList match{
      case Nil => Nil
      case n :: tail => {
        if(num <= n){
          n :: allGTE(num, tail)
        }else{
          allGTE(num, tail)
        }
      }
    }
  }

  def quickSort(aList: List[Int]): List[Int] = {
    aList match{
      case Nil => Nil
      case n :: Nil => n :: Nil
      case n :: tail => {
        quickSort(allLT(n, tail)) ::: n :: Nil ::: quickSort(allGTE(n, tail))
      }
    }
  }

  def sort(aList: List[Int]): List[Int] = {
    quickSort(aList)
  }
}