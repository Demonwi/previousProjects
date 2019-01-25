import Lists._

class TestSuite extends org.scalatest.FunSuite {

  test("oddNumbers properly defined") {
    assert(oddNumbers == List(1, 3, 5))
  }

  test("sumDouble basic test"){
    assert(sumDouble(List(1, 2, 3, 0)) == 12)
  }

  test("sumDouble negative test"){
    assert(sumDouble(List(1, 2, 3, -6)) == 0)
  }

  test("sumDouble empty test"){
    assert(sumDouble(Nil) == 0)
  }

  test("removeZeros basic test"){
    assert(removeZeros(List(1, 2, 3, 0, 4)) == List(1, 2, 3, 4))
  }

  test("removeZeros all zeros test"){
    assert(removeZeros(List(0, 0, 0, 0, 0)) == Nil)
  }

  test("removeZeros no zeros test"){
    assert(removeZeros(List(1, 2, 3, 4)) == List(1, 2, 3, 4))
  }

  test("removeZeros empty test"){
    assert(removeZeros(Nil) == Nil)
  }

  test("countEvens basic test"){
    assert(countEvens(List(2, 0, 3, 2)) == 3)
  }

  test("countEvens negative test"){
    assert(countEvens(List(-2, 0, 3, 2)) == 3)
  }

  test("countEvens no evens test"){
    assert(countEvens(List(-1, 1, 3, 5)) == 0)
  }

  test("countEvens all evens test"){
    assert(countEvens(List(-2, 0, 4, 2)) == 4)
  }

  test("countEvens empty test"){
    assert(countEvens(Nil) == 0)
  }

  test("removeAlternating oddCount test"){
    assert(removeAlternating(List(1, 2, 3, 0, 4)) == List(1, 3, 4))
  }

  test("removeAlternating evenCount test"){
    assert(removeAlternating(List(1, 2, 3, 0, 4, 6)) == List(1, 3, 4))
  }

  test("removeAlternating empty test"){
    assert(removeAlternating(Nil) == Nil)
  }

  test("Helper firstVal basic test"){
    assert(firstVal(List(1, 2, 3, 0, 4)) == 1)
  }

  test("isAscending false test"){
    assert(isAscending(List(1, 2, 3, 0, 4)) == false)
  }

  test("isAscending repetition false test"){
    assert(isAscending(List(1, 2, 3, 3, 5, 4)) == false)
  }

  test("isAscending negative included false test"){
    assert(isAscending(List(-1, -2, 0, 1, 2, 3, 4)) == false)
  }

  test("isAscending true test"){
    assert(isAscending(List(1, 2, 3, 4)) == true)
  }

  test("isAscending repetition true test"){
    assert(isAscending(List(1, 2, 2, 3, 4)) == true)
  }

  test("isAscending negative included true test"){
    assert(isAscending(List(-2, -1, 0, 1, 2, 3, 4)) == true)
  }

  test("addSub oddCount test"){
    assert(addSub(List(1, 2, 3, 0, 4)) == 6)
  }

  test("addSub evenCount test"){
    assert(addSub(List(1, 2, 3, 0, 4, 3)) == 3)
  }

  test("addSub empty test"){
    assert(addSub(Nil) == 0)
  }

  test("alternating basic test"){
    assert(alternate(List (1, 3, 5), List (2, 4, 6)) == List (1, 2, 3, 4, 5, 6))
  }

  test("alternating fail test"){
    assert(alternate(List (1, 3, 5), List (2, 4, 6, 7)) == List (1, 2, 3, 4, 5, 6))
  }

  test("alternating repetition test"){
    assert(alternate(List (1, 3, 5), List (2, 3, 3)) == List (1, 2, 3, 3, 5, 3))
  }

  test("fromTo basic test"){
    assert(fromTo(9, 13) == List(9, 10, 11, 12))
  }

  test("fromTo empty test"){
    assert(fromTo(13, 13) == Nil)
  }

  test("fromTo error test"){
    assert(fromTo(14, 13) == Nil)
  }

  test("insertOrdered basic test"){
    assert(insertOrdered(3, List(1, 2, 4)) == List (1, 2, 3, 4))
  }

  test("insertOrdered duplicate test"){
    assert(insertOrdered(3, List(1, 2, 3, 4)) == List (1, 2, 3, 3, 4))
  }

  test("insertOrdered empty test"){
    assert(insertOrdered(3, Nil) == List (3))
  }

  test("sort basic test"){
    assert(sort(List(4, 3, 2, 1)) == List (1, 2, 3, 4))
  }

  test("sort empty test"){
    assert(sort(Nil) == Nil)
  }

  test("sort repetition test"){
    assert(sort(List(4, 3, 2, 1, 4, 3, 5)) == List (1, 2, 3, 3, 4, 4, 5))
  }

  test("quickSort basic test (Its literally called in sort)"){
    assert(quickSort(List(4, 3, 2, 1)) == List (1, 2, 3, 4))
  }

  test("allLT basic test"){
    assert(allLT(3, List(4, 3, 2, 1)) == List (2, 1))
  }

  test("allGTE basic test"){
    assert(allGTE(3, List(4, 4, 3, 2, 1)) == List (4, 4, 3))
  }
}