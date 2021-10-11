package gettingstarted

object GettingStarted extends App {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def rec(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else rec(n - 1, cur, prev + cur)

    rec(n, 0, 1)
  }

  println(fib(10))

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (as.length - 1 <= n) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  println(isSorted(Array("bird", "dog", "mouse"), (a: String, b: String) => a > b))
  println(isSorted(Array(4, 7, 2), (a: Int, b: Int) => a > b))

}
