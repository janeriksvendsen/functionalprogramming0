package no.embriq.scala.exercises.lists

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(i, is) => i + sum(is)
  }

  def product(ds: List[Int]): Int = ds match {
    case Nil => 1
    case Cons(d, ds) => d * product(ds)
  }

  // returner de n første elementer i lista
  def take[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(i, is) => if (n == 0) Nil else Cons(i, take(is, n-1))
  }

  // returner resten av lista fra pos n.
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(i, is) => if (n == 0) l else drop(is, n-1)
  }

  def length[A](as: List[A]): Int = as match {
    case Nil => 0
    case Cons(i, is) => 1 + length(is)
  }

  // Slå sammen to lister til 1
  def append[A](a1: List[A], a2: List[A]): List[A] = ???

  def reverse[A](l: List[A]): List[A] = ???

  // gjør om en liste av lister til en liste med elementen i sub listen.
  //  List.flatten( List(List(1,2,3), List(4,5,6))) == List(1,2,3,4,5,6)
  def flatten[A](l: List[List[A]]): List[A] = ???

}
