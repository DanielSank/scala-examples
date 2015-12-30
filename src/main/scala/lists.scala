object ListsMain {

  abstract class MyList[+A] {
    def ::[B >: A](newHead: B): NotEmptyList[B] = this match {
      case NotEmptyList(head, tail) => NotEmptyList(newHead, this)
      case Nil => NotEmptyList(newHead, this)
    }
    def get(index: Int): A = this match{
      case Nil => error("oh no")
      case x: NotEmptyList[A] => {
        if (index == 0) x.head
        else x.tail.get(index - 1)
      }
    }
  }

  case class NotEmptyList[A](head: A, tail: MyList[A]) extends MyList[A]
  object Nil extends MyList[Nothing]

  def insertSorted[A <% Ordered[A], B >: A <% Ordered[B]](elem: B, sortedTail: MyList[A]): MyList[B] = sortedTail match {
    case Nil => NotEmptyList(elem, Nil)
    case x: NotEmptyList[A] => {
      if (elem < x.head) NotEmptyList(elem, x)
      else NotEmptyList(x.head, insertSorted(elem, x.tail))
    }
  }

  def isort[A <% Ordered[A]](xs: MyList[A]): MyList[A] = xs match{
    case Nil => Nil
    case x: NotEmptyList[A] => insertSorted(x.head, isort(x.tail))
  }

  def main(args: Array[String]): Unit = {
    val myList = 5 :: 3 :: 4 :: 1 :: Nil
    val sorted = isort(myList)
    println(s"sorted: ${sorted}")
  }
}
