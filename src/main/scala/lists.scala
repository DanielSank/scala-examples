abstract class MyList[+A] {
  def ::[B >: A](newHead: B): NotEmptyList[B] = this match {
    case NotEmptyList(head, tail) => NotEmptyList(newHead, this)
    case MyNil => NotEmptyList(newHead, this)
  }
  def get(index: Int): A = this match{
    case MyNil => error("oh no")
    case x: NotEmptyList[A] => {
      if (index == 0) x.head
      else x.tail.get(index - 1)
    }
  }
  def isEmpty: Boolean = this match {
    case MyNil => true
    case NotEmptyList(head, tail) => false
  }
  def length: Int = {
    lengthRecur(0, this)
  }
  private def lengthRecur[B](total: Int, list: MyList[B]): Int = list match {
    case MyNil => total
    case NotEmptyList(head, tail) => lengthRecur(total + 1, tail)
  }
}

case class NotEmptyList[A](head: A, tail: MyList[A]) extends MyList[A]
object MyNil extends MyList[Nothing]

object ListsMain {

  def insertSorted[A <% Ordered[A], B >: A <% Ordered[B]](elem: B, sortedTail: MyList[A]): MyList[B] = sortedTail match {
    case MyNil => NotEmptyList(elem, MyNil)
    case x: NotEmptyList[A] => {
      if (elem < x.head) NotEmptyList(elem, x)
      else NotEmptyList(x.head, insertSorted(elem, x.tail))
    }
  }

  def isort[A <% Ordered[A]](xs: MyList[A]): MyList[A] = xs match{
    case MyNil => MyNil
    case x: NotEmptyList[A] => insertSorted(x.head, isort(x.tail))
  }

  def main(args: Array[String]): Unit = {
    val myList = 5 :: 3 :: 4 :: 1 :: MyNil
    val sorted = isort(myList)
    println(s"sorted: ${sorted}")
    println(s"length of list = ${sorted.length}")
  }
}
