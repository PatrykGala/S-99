import WorkingWithListsP07._
import org.scalatest.FunSuite

class WorkingWithListsP07Test extends FunSuite {


  test("Flatten a nested list structure. Recursive") {
    val result = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    assert(result == List(1, 1, 2, 3, 5, 8))
  }

  test("Flatten a nested list structure. Tail recursive") {
    val result = flatten2(List(List(1, 1), 2, List(3, List(5, 8))))
    assert(result == List(1, 1, 2, 3, 5, 8))
  }

  test("Eliminate consecutive duplicates of list elements.") {
    val result = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(result == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Eliminate consecutive duplicates of list elements.2") {
    val result = compress2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(result == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Pack consecutive duplicates of list elements into sublists.") {
    val result = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(result == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("Run-length encoding of a list.") {
    val result = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(result == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("Modified run-length encoding.") {
    val result = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(result == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  test("Decode a run-length encoded list.") {
    val result = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(result == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("Run-length encoding of a list (direct solution).") {
    val result = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(result == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("Duplicate the elements of a list.") {
    val r = duplicate(List('a, 'b, 'c, 'c, 'd))
    assert(r == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))

  }

  test("Duplicate the elements of a list a given number of times.") {
    val r = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    assert(r == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("Drop every Nth element from a list.") {
    val r = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(r == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("Split a list into two parts.") {
    val r = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(r == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test(" Extract a slice from a list.") {
    val r = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(r == List('d, 'e, 'f, 'g))
  }

  test("Rotate a list N places to the left.#1") {
    val r = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(r == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  test("Rotate a list N places to the left.#2") {
    val r = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(r == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("Remove the Kth element from a list.") {
    val r = removeAt(1, List('a, 'b, 'c, 'd))
    assert(r == (List('a, 'c, 'd), 'b))
  }


}
