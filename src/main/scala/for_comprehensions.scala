object ForComprehensionMain {
  def main(args: Array[String]) = {
    val l = 5 :: 4 :: 3 :: 2 :: 1 :: Nil
    val lGreaterThanTwo = for (item <- l if item > 2) yield item

    val tuples = for { i <- List.range(1, 4)
         j <- List.range(1, i)} yield (i, j)
    println(tuples)
  }
}
