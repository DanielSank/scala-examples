object StackMain {
  abstract class Stack[+A] {
    def push[B >: A](x: B): Stack[B] = new NonEmptyStack[B](x, this)
    def isEmpty: Boolean
    def top: A
    def pop: Stack[A]
    def isPrefix[B >: A](other: Stack[B]): Boolean = {
      def recur(a: Stack[A], b: Stack[B]): Boolean = {
        // Is a a prefix of b?
        a.isEmpty ||
        a.top == b.top && recur(a.pop, b.pop)
      }
      recur(this, other)
    }
  }
  object EmptyStack extends Stack[Nothing] {
    def isEmpty = true
    def top = error("EmptyStack.top")
    def pop = error("EmptyStack.pop")
  }
  class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
    def isEmpty = false
    def top = elem
    def pop = rest
  }
  def main(args: Array[String]) = {
    val myStack = EmptyStack.push(54)
    val stack1 = myStack.push(6.6)
    val stack2 = myStack.push(3)
    println(stack2.top)
  }
}
