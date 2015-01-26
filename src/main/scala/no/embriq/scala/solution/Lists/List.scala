package no.embriq.scala.solution.Lists

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def toString[A](l: List[A]): String = l match {
    case Nil => ""
    case Cons(x, xs) => x.toString ++ toString(xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def flatten[A](l: List[List[A]]): List[A] = {
    List.foldRight(l, Nil: List[A])(append)
  }

  def sum(l: List[Int]): Int = List.foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = List.foldLeft(l, 1)(_ * _)

  def take[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) Nil else Cons(h, take(t, n - 1))
  }

  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, takeWhile(t, f)) else Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) l else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  def length2[A](l: List[A]): Int = List.foldLeft(l, 0)((i, _) => i + 1)

  def length3[A](l: List[A]): Int = List.foldRight(l, 0)((_, i) => i + 1)

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t, f)) else filter(t, f)
  }

  def filter2[A](l: List[A], f: A => Boolean): List[A] = this.foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter3[A](l: List[A])( f: A => Boolean): List[A] = this.foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def map[A, B](l: List[A], f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t, f))
  }

  def map2[B, A](l: List[A], f: A => B): List[B] = this.foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => List.append(f(h), flatMap(t, f))
  }

  def flatMap2[A, B](l: List[A], f: A => List[B]): List[B] = List.flatten(List.map(l, f))

  def flatMap3[A, B](l: List[A], f: A => List[B]): List[B] = this.foldRight(l, Nil: List[B])((h, t) => List.append(f(h), t))

  //  true iff f == true on all elements
  def forall[A](l: List[A], f: A => Boolean): Boolean = l match {
    case Nil => true
    case Cons(h, t) => if (f(h)) forall(t, f) else false
  }

  def forall2[A](l: List[A], f: A => Boolean): Boolean = foldLeft(l, true)(_ && f(_))

  // true iff f == true on any elements
  def exists[A](l: List[A], f: A => Boolean): Boolean = l match {
    case Nil => false
    case Cons(h, t) => if (f(h)) true else exists(t, f)
  }

  def exists2[A](l: List[A], f: A => Boolean): Boolean = foldLeft(l, false)(_ || f(_))

}
