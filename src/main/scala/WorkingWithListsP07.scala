

object WorkingWithListsP07 {


  def flatten(list: List[Any]): List[Any] = list flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }


  def flatten2(list: List[Any], result: List[Any] = Nil): List[Any] = list match {
    case x :: xs => x match {
      case y: List[_] => flatten2(y ::: xs, result)
      case r => flatten2(xs, result ::: List(r))
    }
    case _ => result
  }

  def compress(list: List[Any], prev: Any = None, result: List[Any] = Nil): List[Any] = list match {
    case x :: xs => if (prev == x) compress(xs, x, result) else compress(xs, x, result ::: List(x))
    case _ => result
  }

  def compress2(list: List[Any], prev: Any = None, result: List[Any] = Nil): List[Any] =
    list.foldRight(List[Any]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }

  def pack[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span {
        _ == list.head
      }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list) map (t => (t.length, t.head)
      )
  }

  def encodeModified[T](list: List[T]): List[Any] =
    pack(list) map { t => if (t.length > 1) (t.length, t.head) else t.head }

  def decode[T](list: List[(Int, T)]): List[T] =
    list.foldRight(List[T]()) { (i, r) => r.:::(List.fill(i._1)(i._2)) }

  def encodeDirect[T](list: List[T], prev: T = None, sum: Int = 0, result: List[(Int, T)] = Nil): List[(Int, T)] = {
    list match {
      case x :: xs => if (prev == x || prev == None) encodeDirect(xs, x, sum + 1, result) else encodeDirect(xs, x, 1, result ::: List((sum, prev)))
      case _ => result ::: List((sum, prev))
    }
  }

  def duplicate[T](list: List[T]): List[T] = list.foldRight(List[T]()) { (i, r) => r.:::(List(i, i)) }

  def duplicateN[T](n: Int, list: List[T]): List[T] = list.foldRight(List[T]()) { (i, r) => r.:::(List.fill(n)(i)) }

  def drop[T](n: Int, list: List[T], index: Int = 1, result: List[T] = Nil): List[T] = list match {
    case x :: xs => if (index == n) drop(n, xs, 1, result) else drop(n, xs, index + 1, result ::: List(x))
    case _ => result
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = (list.take(n), list.drop(n))

  def slice[T](i: Int, j: Int, ls: List[T]) = ls.take(j).drop(i)

  def rotate[T](n: Int, ls: List[T]): List[T] = {
    val tmp = if (n > 0) ls.splitAt(n) else ls.splitAt(ls.length - Math.abs(n))
    tmp._2 ::: tmp._1
  }

  def removeAt[T](n: Int, ls: List[T]): (List[T], _) = {
    val tmp = ls.splitAt(n)
    (tmp._1 ::: tmp._2.tail, tmp._2.head)
  }

  def insertAt[T](elem: T, n: Int, list: List[T], result: List[T] = Nil): List[T] = {
    list match {
      case x :: xs => if (n != 0) insertAt(elem, n - 1, xs, x :: result) else insertAt(elem, n - 1, xs, x :: elem :: result)
      case _ => result.reverse
    }
  }

  def range(n: Int, k: Int): List[_] = {
    if (n <= k) n :: range(n + 1, k) else Nil
  }

}
