package pfp.tools

import scalaz._, Scalaz._

object Exercise {
  // 1. Generalize merge method to work with any Map[A, B]
  // def merge[A, B](m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {

  def merge[A, B: Monoid](m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
    val keys: Iterable[A] = m1.keys ++ m2.keys
    val zero = Monoid[B].zero
    val kvs = keys.map {
      k => k -> (m1.get(k).getOrElse(zero) |+| m2.get(k).getOrElse(zero))
    }
    Map[A, B](kvs.toSeq: _*)
  }


  def merge(m1: Map[String, Int], m2: Map[String, Int]): Map[String, Int] = {
    val keys = m1.keys ++ m2.keys
    val kvs = keys.map {
      k => (k -> (m1.get(k).getOrElse(0) + m2.get(k).getOrElse(0)))
    }
    Map[String, Int](kvs.toSeq: _*)
  }
}
