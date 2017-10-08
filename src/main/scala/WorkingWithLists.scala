

object WorkingWithLists {


  def lastElementOfList(list: List[Any]): Option[Any] =
    list match {
      case Nil => None
      case x :: Nil => Some(x)
      case x :: xs => lastElementOfList(xs)
    }

  def penultimate(list: List[Any]): Option[Any] =
    list match {
      case x :: _ :: Nil => Some(x)
      case x :: xs => penultimate(xs)
      case _ => None
    }


  def nth(n: Int, list: List[Any]): Option[Any] =
    list match {
      case x :: xs => if (n == 0) Some(x) else nth(n - 1, xs)
      case _ => None
    }

  def length(list: List[Any], sum: Int = 0): Int =
    list match {
      case x :: xs => length(xs, sum + 1)
      case _ => sum
    }

  def reverse(list: List[Any], result: List[Any] = Nil): List[Any] = {
    list match {
      case (x :: xs) => reverse(xs, x :: result)
      case _ => result
    }
  }

  def isPalindrome(list: List[Any]): Boolean =
    list == reverse(list)


}
