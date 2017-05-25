package pfp.typeclasses

object Typeclasses {

  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    def apply[A: Show]: Show[A] = implicitly[Show[A]]
    implicit class ShowOps[A: Show](a: A) {
      def show: String = Show[A].show(a)
    }
  }

  class User(val firstName: String, val lastName: String)

  def hello[A](a: A)(implicit sh: Show[A]): String = s"hi ${sh.show(a)}"
  def hello2[A: Show](a: A): String = s"hi ${Show[A].show(a)}"
  def hello3[A: Show](a: A): String = {
    import Show._
    s"hi ${a.show}"
  }

  def main(args: Array[String]): Unit = {
    implicit val UserShow: Show[User] = new Show[User] {
      def show(user: User) = s"${user.firstName} ${user.lastName}"
    }

    val user = new User("John","Yo")
    println(hello(user))
  }

}

