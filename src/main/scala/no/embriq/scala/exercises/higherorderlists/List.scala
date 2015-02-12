package no.embriq.scala.exercises.higherorderlists

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def apply[A](as: scala.List[A]): List[A] = as.foldRight(Nil: List[A])((a: A, b: List[A]) => Cons(a, b))

  // fra høyre til venstre
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = ???

  // fra venstre til høyre
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def sum(ints: List[Int]): Int = ???

  def product(ds: List[Int]): Double = ???

  // returner de n første elementer i lista
  def take[A](l: List[A], n: Int): List[A] = ???

  // returner alle i lista frm til funksjonen returnere true
  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // returner resten av lista fra pos n.
  def drop[A](l: List[A], n: Int): List[A] = ???

  // hopp over de første elementene i listen så lenge funksjonen returnerer true
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def length[A](as: List[A]): Int = ???

  // Slå sammen to lister til 1
  def append[A](a1: List[A], a2: List[A]): List[A] = ???

  def filter[A](l: List[A], f: A => Boolean): List[A] = ???

  def reverse[A](l: List[A]): List[A] = ???

  // gjør om en liste av lister til en liste med elementen i sub listen.
  //  List.flatten( List(List(1,2,3), List(4,5,6))) == List(1,2,3,4,5,6)
  def flatten[A](l: List[List[A]]): List[A] = ???

  // return er en ny liste som er resultatet av f applied på alle elementetn i listen.
  def map[A, B](l: List[A], f: A => B): List[B] = ???

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = ???

  // true iff f == true on all elements
  def forall[A](l: List[A], f: A => Boolean): Boolean = ???

  // true iff f == true on any elements
  def exists[A](l: List[A], f: A => Boolean): Boolean = ???

}
