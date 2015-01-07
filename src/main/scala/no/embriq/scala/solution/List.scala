package no.embriq.scala.solution

import scala.annotation.tailrec

sealed trait List[+A] {
  def take(n: Int): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) Nil else Cons(h, t.take(n - 1))
  }

  def takeWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, t takeWhile f) else Nil
  }

  def drop(n: Int): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) this else t drop (n - 1)
  }

  def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) t dropWhile f else this
  }

  def length: Int = this match {
    case Nil => 0
    case Cons(h, t) => 1 + t.length
  }

  def length2 = foldLeft(0)((i, _) => i + 1)

  def length3 = foldRight(0)((_, i) => i + 1)

//  def append(l: List[A]): List[A] = this match {
//    case Nil => l
//    case Cons(h, t) => Cons(h, t append l)
//  }

  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(h, t) => f(h, t.foldRight(z)(f))
  }

  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Nil => z
    case Cons(h, t) => t.foldLeft(f(z, h))(f)
  }

  def reverse = this.foldLeft(Nil: List[A])((t, h) => Cons(h, t))

  def filter(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, t filter f) else t filter f
  }

  //  def flatten: List[A] = {
  //    case List[A] => this.foldRight(Nil: List[A])(append)
  //  }

  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), t map f)
  }

  def flatMap[B](f: A => List[B]): List[B] = List.flatten(this map f)

  // true iff f == true on all elements
  def forall(f: A => Boolean): Boolean = this match {
    case Nil => true
    case Cons(h, t) => if (f(h)) t forall f else false
  }

  // true iff f == true on any elements
  def exists(f: A => Boolean): Boolean = this match {
    case Nil => false
    case Cons(h, t) => if (f(h)) true else t exists f
  }

}

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

  def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  def product(l: List[Int]): Int = l.foldLeft(1)(_ * _)

  def flatten[A](l: List[List[A]]): List[A] = {
    l.foldRight(Nil: List[A])(append)
  }


  //  def take[A](l: List[A], n: Int): List[A] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) => if (n == 0) Nil else Cons(h, take(t, n - 1))
  //  }

  //  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) => if (f(h)) Cons(h, takeWhile(t, f)) else Nil
  //  }
  //
  //  def drop[A](l: List[A], n: Int): List[A] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) => if (n == 0) l else drop(t, n - 1)
  //  }
  //
  //  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  //  }
  //
  //  def length[A](l: List[A]): Int = l match {
  //    case Nil => 0
  //    case Cons(h, t) => 1 + length(t)
  //  }
  //

  //
  //  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
  //    case Nil => z
  //    case Cons(h, t) => f(h, foldRight(t, z)(f))
  //  }
  //
  //  @tailrec
  //  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  //    case Nil => z
  //    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  //  }
  //  def reverse[A](l: List[A]): List[A] = l.foldLeft(Nil: List[A])((t, h) => Cons(h, t))
  //
  //  def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) => if (f(h)) Cons(h, filter(t, f)) else filter(t, f)
  //  }


  //  def map[A, B](l: List[A], f: A => B): List[B] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) => Cons(f(h), map(t, f))
  //  }
  //
  //  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = ???

  //    l match {
  //    case Nil => Nil
  //    case Cons(h, t) => Cons(f(h), flatMap(t)(f))
  //  }

  // true iff f == true on all elements
  //  def forall[A](l: List[A], f: A => Boolean): Boolean = l match {
  //    case Nil => true
  //    case Cons(h, t) => if (f(h)) forall(t, f) else false
  //  }
  //
  //  // true iff f == true on any elements
  //  def exists[A](l: List[A], f: A => Boolean): Boolean = l match {
  //    case Nil => false
  //    case Cons(h, t) => if (f(h)) true else exists(t, f)
  //  }


}
