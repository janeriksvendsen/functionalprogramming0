

// List er en ADT (Algebraic data type)
sealed trait List[+A];
// Sammensatt av enten Ingenting
case object Nil extends List[Nothing]
// eller et element etterfulgt av en liste
case class Cons[+A](head: A, tail: List[A]) extends List[A]


// Construksjon av en liste
val list0 = Nil // en tom liste
val list1 = Cons(1, Nil) // en liste med et element
val list2 = Cons(2, list1) // en liste med to elementr

// Deconstruksjone av en liste

val head = list2 match {
  case Cons(x, xs) => x
}

// rekursjon
def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(i, is) => i + sum(is)
}

sum(list2)





//Conpanion objekt for å forenkle instansiering
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
