package laziness

trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
  val x = maybeTwice(true, { println("hi"); 1 + 41 })
  println(x)

  def maybeTwice2(b: Boolean, i: => Int) = {
    val j = i
//    lazy val j = i
    if (b) j + j + j else 0
  }
  val x2 = maybeTwice2(true, { println("hi"); 1 + 41 })
  println(x2)
}
