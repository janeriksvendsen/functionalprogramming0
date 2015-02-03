package no.embriq.scala.solution.lists

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

  def flatten[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(h, flatten(t))
  }

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(l: List[Int]): Int = l match {
    case Nil => 1
    case Cons(h, t) => h * product(t)
  }

  def take[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) Nil else Cons(h, take(t, n - 1))
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) l else drop(t, n - 1)
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  def reverse[A](l: List[A]): List[A] = {
    def reverse(acc: List[A], rest: List[A]): List[A] = rest match {
      case Nil => acc
      case Cons(h, t) => reverse(Cons(h, acc), t)
    }

    reverse(Nil, l)
  }

  def reverse2[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse2(t), List(h))
  }

}
