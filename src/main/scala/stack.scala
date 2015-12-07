object MyProgram {
  abstract class Stack[A] {
    def push(x: A): Stack[A] = new NonEmptyStack[A](x, this)
    def isEmpty: Boolean
    def top: A
    def pop: Stack[A]
    def isPrefix(other: Stack[A]): Boolean = {
      def recur(a: Stack[A], b: Stack[A]): Boolean = {
        a.isEmpty ||
        a.top == b.top && recur(a.pop, b.pop)
      }
      recur(this, other)
    }
  }
  class EmptyStack[A] extends Stack[A] {
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
    val myStack = new EmptyStack[Int]
    val stack1 = myStack.push(4)
    val stack2 = myStack.push(3)
    println(stack2.top)
  }
}
