// funksjonen
//def max(x: Int, y: Int): Int = {
//  if (x > y) x
//  else y
//}
//
//max(2, 4)
// trenger ikke spesifisere typer hvis det er gitt.
// er er det opplagt at vi retunerer en Int
//def max2(x: Int, y: Int) = {
//  if (x > y) x
//  else y
//}
// hvis funksjonen kun er et statement kan man droppe { }
//def max3(x: Int, y: Int) =
//  if (x > y) x
//  else y
//max3(2, 4)
// en funksjon kan ha funksjoner
//def factorial(n: Int): Int = {
//  def go(n: Int, acc: Int): Int =
//    if (n <= 0) acc
//    else go(n - 1, n * acc)
//
//  go(n, 1)
//}
//
//factorial(3)
// kan returnere funjsoner
//def functionFactory(n: Int): () => Int = {
//  def identithy(): Int = n
//
//  identithy
//}

//functionFactory(3) // er en funksjone

// funksjoner kan ta andre funksjoner som params
//def applyFunction(n: Int, f: Int => Int): Int = f(n)
def double(n: Int): Int = n * 2
def square(n: Int): Int = n * n
def cube(n: Int): Int = n * n * n
//applyFunction(3, double)
//applyFunction(3, square)
//applyFunction(3, cube)
//// anonymous functions
//// full deklarasjon
//applyFunction(4, (x: Int) => {-x})
//
//// enklere
//applyFunction(4, x => -x)
//
//// enklest
//applyFunction(4, -_)
// composisjon av funksjoner.
val composed = double _ compose cube _ compose square
// er samme som
def equaledToComposed(x: Int) = double(cube(square(x)))
// kommer tilbake med mer spennende komponering i en senere WS
composed(3)
equaledToComposed(3)
// funksjoner i data strukturer
val functions = List(double _, square _ , cube _)
val listComposed = functions reduce(_ compose _)
listComposed(3)




// Typen liste
sealed trait List[+A]

// er enten tne tom liste
case object Nil extends List[Nothing]

// eller et element etterfulgt av en tom liste
case class Cons[+A](head: A, tail: List[A]) extends List[A]


















