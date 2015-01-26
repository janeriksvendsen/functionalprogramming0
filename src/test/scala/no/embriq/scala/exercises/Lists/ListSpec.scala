package no.embriq.scala.exercises.Lists

import no.embriq.scala.exercises.{PendingIfUnimplemented, _}
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers with PendingIfUnimplemented {

  val list123 = List(1, 2, 3)
  val list456 = List(4, 5, 6)

  "List fold right with cons" should "return the same list" in {
    List.foldRight(list123, Nil: List[Int])(Cons(_, _)) shouldBe list123
  }

  "List fold left with cons" should "return the same list reversed" in {
    List.foldLeft(list123, Nil: List[Int])((xs, x) => Cons(x, xs)) shouldBe List(3, 2, 1)
  }

  "take" should "return a list of the n first elements " in {
    List.take(list123, 2) shouldBe List(1, 2)
  }

  "drop" should "return a list vhere the n first elements are removed" in {
    List.drop(list123, 2) shouldBe List(3)
  }

  "append of 2 lists" should "return a new list contaning the elements from the 2 lists" in {
    List.append(list123, list456) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "length" should "return the lengt of the list" in {
    List.length(list123) shouldBe 3
  }

  "drop while" should "drop start of list while functions evaluate to true" in {
    List.dropWhile(list123, (x: Int) => x < 2) shouldBe List(2, 3)
  }

  "take while" should "return start of list until functions evaluate to true" in {
    List.takeWhile(list123, (x: Int) => x < 2) shouldBe List(1)
  }

  "sum" should "return the som of all elements in the list" in {
    List.sum(list123) shouldBe 6
    List.sum(list456) shouldBe 15
  }

  "product " should "return the product of all elements in the list" in {
    List.product(list123) shouldBe 6
    List.product(list456) shouldBe 120
  }

  "List revers" should "return the same list reversed" in {
    List.reverse(list123) shouldBe List(3, 2, 1)
  }

  "exist" should " retur true when 3 is in the list" in {
    List.exists(list123, (x: Int) => x == 3) shouldBe true
    List.exists(list123, (x: Int) => x == 4) shouldBe false
  }

  "for all" should "retur true only the list has values under 4" in {
    List.forall(list123, (x: Int) => x == 3) shouldBe false
    List.forall(list123, (x: Int) => x < 4) shouldBe true
  }

  "flatten" should "merge all list in a list to one list" in {
    List.flatten(List(list123, list456)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "filter" should "reteurn a new list with all the elements that evaluates to true" in {
    List.filter(list123, (x: Int) => x > 2) shouldBe List(3)
  }

  "map" should "return a new list with the function applied" in {
    List map(list123, (x: Int) => x + 3) shouldBe List(4, 5, 6)
    List.map(list123, (x: Int) => List(x, x)) shouldBe List(List(1, 1), List(2, 2), List(3, 3))
  }

  "flatmap" should "return a flattend list" in {
    List.flatMap(list123, (x: Int) => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

}


