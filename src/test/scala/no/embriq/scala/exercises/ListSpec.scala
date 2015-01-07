import no.embriq.scala.exercises.List._
import no.embriq.scala.exercises._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers with PendingIfUnimplemented {

  val list123 = List(1, 2, 3)
  val list456 = List(4, 5, 6)

  "List constructor" should "return same as Cons" in {
    list123 shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }

  "List fold right with cons" should "return the same list" in {
    foldRight(list123, Nil: List[Int])(Cons(_, _)) shouldBe list123
  }


  "List fold left with cons" should "return the same list reversed" in {
    foldLeft(list123, Nil: List[Int])((xs, x) => Cons(x, xs)) shouldBe List(3, 2, 1)
  }

  "drop" should "return a list vhere the n first elements are removed" in {
    drop(list123, 2) shouldBe List(3)
  }

  "append of 2 lists" should "return a new list contaning the elements from the 2 lists" in {
    append(list123, list456) shouldBe List(1, 2, 3, 4, 5, 6)
  }

}


