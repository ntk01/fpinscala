package datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List extends App {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

  println(tail(List(1, 2)))

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, xs) => Cons(h, xs)
    }

  println(setHead(List(1, 2, 3), 4))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  println(drop(List(1, 2, 3, 4, 5), 2))

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  println(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3))

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  println(init(List(1, 2, 3)))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  println(foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  println(length(List(1, 2, 3)))

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  println(sum2(List(1, 2, 4)))

  def product2(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  println(product2(List(1.1, 2.2, 3.5)))

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  println(length2(List(1, 2)))

}
