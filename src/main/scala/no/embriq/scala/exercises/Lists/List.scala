package no.embriq.scala.exercises.lists

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

//  def apply[A](as: scala.List[A]): List[A] =  as.foldRight(Nil:List[A])( (a:A, b:List[A]) => Cons(a,b))

  def sum(ints: List[Int]): Int = ???

  def product(ds: List[Int]): Double = ???

  // returner de n første elementer i lista
  def take[A](l: List[A], n: Int): List[A] = ???

  // returner resten av lista fra pos n.
  def drop[A](l: List[A], n: Int): List[A] = ???

  def length[A](as: List[A]): Int = ???

  // Slå sammen to lister til 1
  def append[A](a1: List[A], a2: List[A]): List[A] = ???

  def reverse[A](l: List[A]): List[A] = ???

  // gjør om en liste av lister til en liste med elementen i sub listen.
  //  List.flatten( List(List(1,2,3), List(4,5,6))) == List(1,2,3,4,5,6)
  def flatten[A](l: List[List[A]]): List[A] = ???

}
