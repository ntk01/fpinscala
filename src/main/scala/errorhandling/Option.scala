package errorhandling

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option extends App {
//  println(Some(2).map(_ + 1))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
//  println(variance(List(1.0, 2.0, 3.0)))

  def map2[A, B, C](a: Option[A], b: Option[B])(
      f: (A, B) => C
  ): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }
  println(map2(Some(1), Some(2))(_ + _))
//  println(map2(Some(1), None)(_ + _))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case Nil    => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
    }
  }
  println(sequence(List(Some(1), Some(2))))

}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _               => None
  }

}
