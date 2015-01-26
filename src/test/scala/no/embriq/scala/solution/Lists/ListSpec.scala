package no.embriq.scala.solution.Lists

import no.embriq.scala.exercises.PendingIfUnimplemented
import no.embriq.scala.solution.Lists
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers with PendingIfUnimplemented {

  val list123 = Lists.List(1, 2, 3)
  val list456 = Lists.List(4, 5, 6)

  "List constructor" should "return same as Cons" in {
    list123 shouldBe Cons(1, Cons(2, Cons(3, Lists.Nil)))
  }

  "List fold right with cons" should "return the same list" in {
    Lists.List.foldRight(list123, Lists.Nil: Lists.List[Int])(Cons(_, _)) shouldBe list123
  }

  "List fold left with cons" should "return the same list reversed" in {
    Lists.List.foldLeft(list123, Lists.Nil: Lists.List[Int])((xs, x) => Cons(x, xs)) shouldBe Lists.List(3, 2, 1)
  }

  "take" should "return a list of the n first elements " in {
    Lists.List.take(list123, 2) shouldBe Lists.List(1, 2)
  }

  "drop" should "return a list vhere the n first elements are removed" in {
    Lists.List.drop(list123, 2) shouldBe Lists.List(3)
  }

  "append of 2 lists" should "return a new list contaning the elements from the 2 lists" in {
    Lists.List.append(list123, list456) shouldBe Lists.List(1, 2, 3, 4, 5, 6)
  }

  "length" should "return the lengt of the list" in {
    Lists.List.length(list123) shouldBe 3
    Lists.List.length2(list123) shouldBe 3
    Lists.List.length3(list123) shouldBe 3
  }

  "drop while" should "drop start of list while functions evaluate to true" in {
    Lists.List.dropWhile(list123, (x: Int) => x < 2) shouldBe Lists.List(2, 3)
  }

  "take while" should "return start of list until functions evaluate to true" in {
    Lists.List.takeWhile(list123, (x: Int) => x < 2) shouldBe Lists.List(1)
  }

  "sum" should "return the som of all elements in the list" in {
    Lists.List.sum(list123) shouldBe 6
    Lists.List.sum(list456) shouldBe 15
  }

  "product " should "return the product of all elements in the list" in {
    Lists.List.product(list123) shouldBe 6
    Lists.List.product(list456) shouldBe 120
  }

  "List revers" should "return the same list reversed" in {
    Lists.List.reverse(list123) shouldBe Lists.List(3, 2, 1)
  }

  "exist" should " retur true when 3 is in the list" in {
    Lists.List.exists(list123, (x: Int) => x == 3) shouldBe true
    Lists.List.exists(list123, (x: Int) => x == 4) shouldBe false

    Lists.List.exists2(list123, (x: Int) => x == 3) shouldBe true
    Lists.List.exists2(list123, (x: Int) => x == 4) shouldBe false
  }

  "for all" should "retur true only the list has values under 4" in {
    Lists.List.forall(list123, (x: Int) => x == 3) shouldBe false
    Lists.List.forall(list123, (x: Int) => x < 4) shouldBe true

    Lists.List.forall2(list123, (x: Int) => x == 3) shouldBe false
    Lists.List forall2(list123, (x: Int) => x < 4) shouldBe true
  }

  "flatten" should "merge all list in a list to one list" in {
    Lists.List.flatten(Lists.List(list123, list456)) shouldBe Lists.List(1, 2, 3, 4, 5, 6)
  }

  "filter" should "reteurn a new list with all the elements that evaluates to true" in {
    Lists.List.filter(list123, (x: Int) => x > 2) shouldBe Lists.List(3)
    Lists.List.filter2(list123, (x: Int) => x > 2) shouldBe Lists.List(3)
    Lists.List.filter2(list123, (x: Int) => x < 2) shouldBe Lists.List(1)
  }

  "map" should "return a new list with the function applied" in {
    Lists.List map(list123, (x: Int) => x + 3) shouldBe Lists.List(4, 5, 6)
    Lists.List map2(list123, (x: Int) => x + 3) shouldBe Lists.List(4, 5, 6)

    Lists.List.map(list123, (x: Int) => Lists.List(x, x)) shouldBe Lists.List(Lists.List(1, 1), Lists.List(2, 2), Lists.List(3, 3))
  }

  "flatmap" should "return a flattend list" in {
    Lists.List.flatMap(list123, (x: Int) => Lists.List(x, x)) shouldBe Lists.List(1, 1, 2, 2, 3, 3)
    Lists.List.flatMap2(list123, (x: Int) => Lists.List(x, x)) shouldBe Lists.List(1, 1, 2, 2, 3, 3)
    Lists.List.flatMap3(list123, (x: Int) => Lists.List(x, x)) shouldBe Lists.List(1, 1, 2, 2, 3, 3)
  }

}


