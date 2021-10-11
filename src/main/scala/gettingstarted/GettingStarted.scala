package gettingstarted

object GettingStarted extends App {

  def fib(n: Int): Int = {
    def rec(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else rec(n - 1, cur, prev + cur)
    rec(n, 0, 1)
  }

  println(fib(10))

}
