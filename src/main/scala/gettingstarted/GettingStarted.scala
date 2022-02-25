package gettingstarted

import scala.annotation.tailrec

object GettingStarted extends App {

  def fib(n: Int): Int = {
    @tailrec
    def rec(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else rec(n - 1, cur, prev + cur)

    rec(n, 0, 1)
  }

  println(fib(10))

  def isSorted[A](a: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (a.length - 1 <= n) true
      else if (gt(a(n), a(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  println(isSorted(Array("bird", "dog", "mouse"), (a: String, b: String) => a > b))
  println(isSorted(Array(4, 7, 2), (a: Int, b: Int) => a < b))

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def curry2[A, B, C, D](f: (A, B, C) => D): A => B => C => D =
    a => b => c => f(a, b, c)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def uncurry2[A, B, C, D](f: A => B => C => D): (A, B, C) => D =
    (a, b, c) => f(a)(b)(c)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    a => g(f(a))

}
