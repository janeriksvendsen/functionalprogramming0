package no.embriq.scala.exercises.lists

import no.embriq.scala.exercises.PendingIfUnimplemented
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers with PendingIfUnimplemented {

  val list123 = List(1, 2, 3)
  val list456 = List(4, 5, 6)

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

  "flatten" should "merge all list in a list to one list" in {
    List.flatten(List(list123, list456)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

}


