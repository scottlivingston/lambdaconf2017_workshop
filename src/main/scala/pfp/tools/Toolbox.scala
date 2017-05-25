package pfp.tools

import scala.language.higherKinds
import scalaz._, Scalaz._, concurrent._

object Toolbox {

  def main(args: Array[String]): Unit = {
    val problem1: Task[Task[Int]] = Task.delay(Task.delay(10))
    val solution1: Task[Int] = problem1.join

    val problem2: List[Task[Int]] = List(Task.now(10), Task.now(20))
    val solution2: Task[List[Int]] = problem2.sequence

    case class User(id: Int, login: String)
    val ids = List(1, 2, 3)
    val fetchUser: Int => Task[User] = id => Task.delay(User(id, s"login $id"))
    val users: Task[List[User]] = ids.traverse(fetchUser)
  }

  def main3(args: Array[String]): Unit = {
    //some two ints
    def sum1(i1: Int, i2: Int): Int = i1 - i2 // this is wrong, but it compiles!
    def sum2[A](i1: A, i2: A): A = i1 // too generic, we cant do anything with these As
    def sum3[A: Semigroup](a1: A, a2: A): A = a1 |+| a2 // |+| is from Semigroup
    def sum4(list: List[Int]): Int = ??? //Lots of ways to implement that are not what you actually want.
    def sum5[A: Monoid](list: List[A]): A = list.fold(Monoid[A].zero)(_ |+| _)
    def sum[F[_]: Foldable, A: Monoid](f: F[A]): A = f.fold

    println("--------->>> " + sum(List(1, 2, 3)) )
    println("--------->>> " + sum(List("a", "b")) )
  }


  def main2(args: Array[String]): Unit = {
    val plus: (Int, Int) => Int = _ + _

    def plusOperation[F[_]: Apply](f1: F[Int], f2: F[Int]): F[Int] = {
      val temp: F[Int => Int] = f1.map(plus.curried)
      Apply[F].ap(f2)(temp)
      /* or */
      Apply[F].apply2(f1, f2)(plus)
      /* or */
      (f1 |@| f2)(plus)
      /* or */
      (f1 |@| f2)(_ + _)
    }

    def plusOperation2[F[_]: Applicative](f1: F[Int], f2: Int): F[Int] = {
      (f1 |@| f2.point[F])(_ + _)
    }

    def plusOperationF[F[_]: Applicative](seed: String, f1: F[Int], f2: String => F[Int]): F[Int] = {
      (f1 |@| f2(seed))(_ + _)
    }

    def plusOperationF2[F[_]: Bind](seed: F[String], f1: F[Int], f2: String => F[Int]): F[Int] = {
      (f1 |@| Bind[F].bind(seed)(f2))(_ + _)
      /* or */
      (f1 |@| (seed >>= f2))(_ + _)
      /* or */
      seed >>= (s => f1 >>= (a => f2(s).map(b => plus(a, b))))
      /* or */
      for {
        s <- seed
        a <- f1
        b <- f2(s)
      } yield plus(a, b)
    }


    val i1 = 10
    val i2 = 20
    println( plusOperation(i1.some, i2.some) )
    println( plusOperation2(i1.some, i2) )
    println( plusOperationF[Option]("hi", i1.some, _.length.some) )
    println( plusOperationF2[Option]("hi".some, i1.some, _.length.some) )
  }

  def main1(args: Array[String]): Unit = {
    val length: String => Int = _.length
    def lengthOperation[F[_]: Functor](str: F[String]): F[Int] = str.map(length)


    val str = "hello"
    lengthOperation[Id](str)

    lengthOperation(str.some)

    val str2: Throwable \/ String = str.right
    type Error[A] = Throwable \/ A
    lengthOperation[Error](str2)
    lengthOperation[Throwable \/ ?](str2)
  }
}
