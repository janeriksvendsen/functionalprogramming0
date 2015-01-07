package no.embriq.scala.exercises

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ???

  def product(ds: List[Double]): Double = ???

  def take[A](l: List[A], n: Int): List[A] = ???

  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def length[A](as: List[A]): Int = ???

  def append[A](a1: List[A], a2: List[A]): List[A] = ???

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = ???

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def filter[A](l: List[A], f: A => Boolean): List[A] = ???

  def reverse[A](l: List[A]): List[A] = ???

  def flatten[A](l: List[List[A]]): List[A] = ???

  def map[A, B](l: List[A], f: A => B): List[B] = ???

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

  // true iff f == true on all elements
  def forall[A](f: A => Boolean): Boolean = ???

  // true iff f == true on any elements
  def exists[A](f: A => Boolean): Boolean = ???

}
