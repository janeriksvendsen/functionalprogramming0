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


  def sum(l: List[Int]): Int = List.foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = List.foldLeft(l, 1)(_ * _)

  def length[A](l: List[A]): Int = List.foldLeft(l, 0)((i, _) => i + 1)

  def length2[A](l: List[A]): Int = List.foldRight(l, 0)((_, i) => i + 1)

  def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  def append[A](a1: List[A], a2: List[A]): List[A] = List.foldRight(a1, a2)((a: A, as: List[A]) => Cons(a, as))

  // egendlig samme som over men gidder ikke å navne parameterne
  def append2[A](a1: List[A], a2: List[A]): List[A] = List.foldRight(a1, a2)(Cons(_, _))

  // Skal ta en liste av lister og gjøre om til en "flat" liste.
  // det blir i praksis å append alle listene sammen til en liste.
  def flatten[A](l: List[List[A]]): List[A] = List.foldRight(l, Nil: List[A])(append)

  // har bare ekspandert append funksjonen for å vise hvodean det ser ut.
  def flatten2[A](l: List[List[A]]): List[A] = List.foldRight(l, Nil: List[A])(List.foldRight(_, _)(Cons(_, _)))

  def flatten3[A](l: List[List[A]]): List[A] = {
    def append(l1: List[A], l2: List[A]): List[A] = l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }

    l match {
      case Nil => Nil
      case Cons(h, t) => append(h, flatten3(t))
    }
  }

  def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, t) else filter(t, f)
  }

  def filter2[A](l: List[A], f: A => Boolean): List[A] = this.foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // from scratch
  def map[B, A](l: List[A], f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t, f))
  }

  def map2[B, A](l: List[A], f: A => B): List[B] = this.foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] = List.flatten(List.map(l, f))

  def flatMap2[A, B](l: List[A], f: A => List[B]): List[B] = this.foldRight(l, Nil: List[B])((a, bs) => List.append(f(a), bs))

  //  true iff f == true on all elements
  def forall[A](l: List[A], f: A => Boolean): Boolean = foldLeft(l, true)(_ && f(_))

  // true iff f == true on any elements
  def exists[A](l: List[A], f: A => Boolean): Boolean = foldLeft(l, false)(_ || f(_))

}
