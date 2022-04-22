package datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List extends App {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

//  println(x)

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil         => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

//  println(tail(List(1, 2)))

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil         => sys.error("setHead on empty list")
      case Cons(_, xs) => Cons(h, xs)
    }

//  println(setHead(List(1, 2, 3), 4))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

//  println(drop(List(1, 2, 3, 4, 5), 2))

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

//  println(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3))

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil          => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }

//  println(init(List(1, 2, 3)))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    println(as)
    println(z)
    println("================================")
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  //  println(foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

//  println(length(List(1, 2, 3)))

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
//    println(l)
//    println(z)
//    println("==========================")
    l match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum2(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

//  println(sum2(List(1, 2, 4)))

  def product2(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

//  println(product2(List(1.1, 2.2, 3.5)))

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

//  println(length2(List(1, 2)))

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

//  println(reverse(List(1, 2, 3)))

  // exercise: 3.13
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

//  println(foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _))

  // exercise: 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, b) => Cons(a, b))

//  println(appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)))

  // exercise: 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(appendViaFoldRight)

//  println(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

  // exercise: 3.16
  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

//  println(incrementEach(List(1, 2, 3)))

  // exercise: 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

//  println(doubleToString(List(1.1, 2.3, 4.45)))

  // exercise: 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

//  println(map(List(1, 2, 3))(a => a * 2))

  // exercise: 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

//  println(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))

  // exercise: 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

//  println(flatMap(List(1, 2, 3))(i => List(i, i + 1)))

  // exercise: 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

//  println(filterViaFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0))

}
