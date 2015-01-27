package no.embriq.scala.solution.higherorderlists

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = List.foldRight(a1, a2)((a: A, as: List[A]) => Cons(a, as))

  def append2[A](a1: List[A], a2: List[A]): List[A] = List.foldRight(a1, a2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = List.foldRight(l, Nil: List[A])(append)

  def sum(l: List[Int]): Int = List.foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int =  List.foldLeft(l, 1)(_ * _)

  def take[A](l: List[A], n: Int): List[A] = ???
  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = ???
  def drop[A](l: List[A], n: Int): List[A] = ???
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def length[A](l: List[A]): Int = List.foldLeft(l, 0)((i, _) => i + 1)

  def length2[A](l: List[A]): Int = List.foldRight(l, 0)((_, i) => i + 1)

  def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  def filter[A](l: List[A], f: A => Boolean): List[A] = this.foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter1[A](l: List[A])(f: A => Boolean): List[A] = this.foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def map[B, A](l: List[A], f: A => B): List[B] = this.foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] = List.flatten(List.map(l, f))

  def flatMap2[A, B](l: List[A], f: A => List[B]): List[B] = this.foldRight(l, Nil: List[B])((h, t) => List.append(f(h), t))

  //  true iff f == true on all elements
  def forall[A](l: List[A], f: A => Boolean): Boolean = foldLeft(l, true)(_ && f(_))

  // true iff f == true on any elements
  def exists[A](l: List[A], f: A => Boolean): Boolean = foldLeft(l, false)(_ || f(_))

}
