package laziness

import laziness.Stream.{cons, empty}

import scala.annotation.tailrec

trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
//      Seq.appendedなので、計算量的にはO(N)な気がするので、実質reverseするのと変わらないかも。
      case Cons(h, t) => loop(t(), acc :+ h())
//      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }
//    loop(this, List()).reverse
    loop(this, List())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if 1 <= n => cons(h(), t().take(n - 1))
    case _                    => empty
  }

//  n = 2: Cons(t: => Stream[A]).drop(2 - 1)
//  n = 1: Cons(t: => Stream[A]).drop(1 - 1)
//  n = 0: this(要素はheadが2個削れたStream[A])
  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if 1 <= n => t().drop(n - 1)
    case _                    => this
  }

//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) =>
//      if (p(h())) cons(h(), t().takeWhile(p))
//      else t().takeWhile(p)
//    case _ => this
//  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
//  def foldRight[B](z: => B)(f: (A, B) => B): B = this match { これで書くとStream全てを評価することになる
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

//  def headOption: Option[A] = foldRight(Option[A])((h, _) => Option(h))
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](p: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(p(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  // Covariant type A occurs in contravariant position in type Stream[A] of value p
//  def append(p: => Stream[A]): Stream[A] =
//    foldRight(this)((h, t) => cons(cons(this, h), t))
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  // Type mismatch. Required: Stream[B], found: Stream[Any]
//  def flatMap[B](p: A => B): Stream[B] =
//    foldRight(empty[B])((h, t) => cons(h, t.flatMap(p)))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

//  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

//  def fibs(a: Int, b: Int): Stream[Int] = this match {
//    case Cons(h, t) => this.append(a + b)
//    case Empty      => cons(0, cons(1, fibs(0, 1)))
//  }
//  val fibs = {
//    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
//    go(0, 1)
//  }

//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = this match {
//    case Cons(h, t) => if (f(z)) cons(h, t.unfold(z)(f)) else cons(h, t)
//    case _          => empty
//  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => Empty
  }

//  val fibsViaUnfold = {
//    def go(c: Stream[Int]): Stream[Int] =
//      unfold(c)(_ => Option(Int, Stream[Int]))
//    go(cons(0, cons(1, empty)))
//  }
//  val fibsViaUnfold = unfold((0, 1)) { case (f0, f1) =>
//    Some((f0, (f1, f0 + f1)))
//  }
//  val fibsViaUnfold =
//    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](
      s2: Stream[B]
  )(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) =>
        Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) =>
        Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll { case (h, h2) =>
      h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def empty[A]: Stream[A] = Empty

  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()
//  if2(1 < 2, () => println("a"), () => println("b"))

  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse
//  if3(1 < 2, () => println("a"), () => println("b"))

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
//  val x = maybeTwice(true, { println("hi"); 1 + 41 })
//  println(x)

  def maybeTwice2(b: Boolean, i: => Int) = {
//    val j = i
    lazy val j = i
    //    if (b) i + i else 0
    if (b) j + j else 0
  }
//  val x2 = maybeTwice2(true, { println("hi"); 1 + 41 })
//  println(x2)

  def p[A](v: A): A = {
    println(v)
    v
  }
  val s = cons(p(1), cons(p(2), cons(p(3), empty)))
  val t = cons(p(4), cons(p(5), cons(p(6), empty)))
//  println(s.forAll(_ < 4))
  val u = Stream(1, 2, 3)
  println(u.tails.map(_.toList).toList)

}
