package laziness

import laziness.Stream.cons

import scala.annotation.tailrec

trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
//      なんとなく最後にreverseつけたくなかったので書き直してみた
//      ただ、Seq.appendedなので、計算量的にはO(N)な気がするので、実質reverseするのと変わらないかも。
      case Cons(h, t) => loop(t(), acc :+ h())
//      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }
//    loop(this, List()).reverse
    loop(this, List())
  }

  // 解答一行削れそうな気がする
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if 1 <= n => cons(h(), t().take(n - 1))
    case _                    => Empty
  }

//  n = 2: Cons(t: => Stream[A]).drop(2 - 1)
//  n = 1: Cons(t: => Stream[A]).drop(1 - 1)
//  n = 0: this(要素はheadが2個削れたStream[A])
  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if 1 <= n => t().drop(n - 1)
    case _                    => this
  }

//  最初takeWhileのつもりで書いたけど、これは多分やってることfilterだ。解答見て読み違えてることに気づいた。
  def filter(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      if (p(h())) cons(h(), t().takeWhile(p))
      else t().takeWhile(p)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => Empty
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
//  val x = maybeTwice(true, { println("hi"); 1 + 41 })
//  println(x)

  def maybeTwice2(b: Boolean, i: => Int) = {
//    val j = i
//    lazy val j = i
    if (b) i + i else 0
//    if (b) j + j else 0
  }
  val x2 = maybeTwice2(true, { println("hi"); 1 + 41 })
  println(x2)

  def p[A](v: A): A = {
    println(v)
    v
  }
  val s = cons(p(1), cons(p(2), cons(p(3), Empty)))
//  println(s.toList)
  val t = cons(p(1), cons(p(2), cons(p(3), Empty)))
//  println(t.take(2))
//  println(t.take(2).toList)
  val u = cons(p(1), Empty)
//  println(u.take(4).toList)
  val v = cons(p(1), cons(p(2), cons(p(3), Empty)))
//  println(v.drop(2).toList)
  val w = cons(p(1), cons(p(2), cons(p(3), Empty)))
//  println(w.takeWhile(_ <= 2).toList)

}
