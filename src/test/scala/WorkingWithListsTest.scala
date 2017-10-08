import WorkingWithLists._
import org.scalatest.FunSuite

import scala.language.postfixOps


class WorkingWithListsTest extends FunSuite {


  test("Find the last element of a list.") {
    val lastElement = lastElementOfList(List(1, 1, 2, 3, 5, 8))
    assert(lastElement.get == 8)
  }

  test("lastElementOfList: it should return None if Nil") {
    val lastElement = lastElementOfList(Nil)
    assert(lastElement.isEmpty)
  }

  test("Find the last but one element of a list.") {
    val result = penultimate(List(1, 1, 2, 3, 5, 8))
    assert(result.get == 5)
  }
  test("penultimate: it should return first if two elements in list") {
    val result = penultimate(List("a"))
    assert(result.isEmpty)
  }
  test("penultimate: it should return None if One element in list") {
    val result = penultimate(List("a"))
    assert(result isEmpty)
  }
  test("penultimate: it should return None if zero element in list") {
    val result = penultimate(List())
    assert(result.isEmpty)
  }

  test("Find the Kth element of a list.") {
    val result = nth(2, List(1, 2, 3))
    assert(result.get == 3)
  }

  test("Find the Kth element of a list.: Should return None if not exist") {
    val result = nth(2, List(1, 2))
    assert(result isEmpty)
  }
  test("Find the Kth element of a list.: Should return None if empty list") {
    val result = nth(2, List())
    assert(result isEmpty)
  }
  test("Find the Kth element of a list.: Should return None if n is too large") {
    val result = nth(123, List(1, 2, 3, 4, 5))
    assert(result isEmpty)
  }

  test("Find the number of elements of a list. Should return 3 if List(1,2,3)") {
    val result = length(List(1, 2, 3))
    assert(result == 3)
  }

  test("Find the number of elements of a list. Should return 0 if List()") {
    val result = length(List())
    assert(result == 0)
  }

  test("Reverse a list.") {
    val list = List(1, 1, 2, 3, 5, 8)
    val result = reverse(list)
    assert(result == list.reverse)
  }
  test("Reverse an empty list.") {
    val list = List()
    val result = reverse(list)
    assert(result == list.reverse)
  }
  test("Find out whether a list is a palindrome.") {
    val result = isPalindrome(List(1, 2, 3, 2, 1))
    assert(result == true)
  }

  test("Find out whether a list is a palindrome2.") {
    val result = isPalindrome(List("1", 2, 2, "1"))
    assert(result == true)
  }

}
