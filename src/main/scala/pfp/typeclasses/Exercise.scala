package pfp.typeclasses

object Exercise {
 
  /** Typeclasses */
  // 1. Implement Equalz[A] typeclass that has method def eq(a1: A, a2: A): Boolean
  trait Equalz[A] {
    def eq2(a1: A, a2: A): Boolean
  }

  // 2. Write EqualzOps & Equalz companion object with apply method
  object Equalz {
    def apply[A: Equalz]: Equalz[A] = implicitly[Equalz[A]]
    implicit class EqualzOps[A: Equalz](a1: A) {
      def eq2(a2: A) = Equalz[A].eq2(a1, a2)
    }

    // 3. Write instance for String and User
    implicit val StringEqualz = new Equalz[String] {
      override def eq2(a1: String, a2: String): Boolean = a1 == a2
    }
    implicit val UserEqualz = new Equalz[User] {
      override def eq2(a1: User, a2: User): Boolean = a1.login.eq2(a2.login)
    }

    // 4. if A & B have instance for Equalz, can u write generic instance for Tuple (A, B)
    implicit def TupleEqualz[A: Equalz, B: Equalz] = new Equalz[(A, B)] {
      override def eq2(a1: (A, B), a2: (A, B)): Boolean = a1._1.eq2(a2._1) && a1._2.eq2(a2._2)
    }
  }

  // 3.
  case class User(login: String)



}
