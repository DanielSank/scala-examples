object VarianceMain {
  // Take a look at the compiler errors here.
  // Try fixing it by making the type parameter of Foo covariant
  // and see what happens.

  class Foo[A] {
    def func(a: A) = {}
  }

  def main(args: Array[String]): Unit = {
    val foo = new Foo[String]
    val bar: Foo[Any] = foo
  }
}
