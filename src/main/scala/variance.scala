object VarianceMain {

  class Foo[A] {
    def func(a: A) = {}
  }

  def main(args: Array[String]): Unit = {
    // Uncomment the next two lines, then take a look at the compiler errors.
    // Try fixing it by making the type parameter of Foo covariant.

    // val foo = new Foo[String]
    // val bar: Foo[Any] = foo

    println("This example doesn't actually do anything.")
    println("Look at the source code to learn about variance.")
  }
}
