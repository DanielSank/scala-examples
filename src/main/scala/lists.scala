object ListsMain {
  abstract class MyList {
    def ::(newHead: Int): NotEmptyList = this match {
      case NotEmptyList(head, tail) => NotEmptyList(newHead, this)
      case Nil => NotEmptyList(newHead, this)
    }
    def isEmpty: Boolean = this match {
      case NotEmptyList(head, tail) => false
      case Nil => true
    }
    def get(index: Int): Int = this match{
      case Nil => -1
      case x: NotEmptyList => {
        if (index == 0) x.head
        else x.tail.get(index - 1)
      }
    }
  }
  case class NotEmptyList(head: Int, tail: MyList) extends MyList
  case object Nil extends MyList

  def insert(elem: Int, sortedTail: MyList): MyList = sortedTail match {
    case Nil => NotEmptyList(elem, Nil)
    case x: NotEmptyList => {
      if (elem < x.head) NotEmptyList(elem, x)
      else NotEmptyList(x.head, insert(elem, x.tail))
    }
  }
  def isort(xs: MyList): MyList = xs match{
    case Nil => Nil
    case x: NotEmptyList => insert(x.head, isort(x.tail))
  }
  def main(args: Array[String]): Unit = {
    val myList = new NotEmptyList(5,
        new NotEmptyList(3,
            new NotEmptyList(4,
                new NotEmptyList(1, Nil))))
    val sorted = isort(myList)
    println(s"sorted: ${sorted}")
  }
}
