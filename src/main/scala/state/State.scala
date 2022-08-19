package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG extends App {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  println(n1)
  println(rng2)
  val (n2, rng3) = rng2.nextInt
  println(n2)
  println(rng3)
}
