// funksjoner kan ta andre funksjoner som params
def applyFunction(n: Int, f: Int => Int): Int = f(n)
def double(n: Int): Int = n * 2
def square(n: Int): Int = n * n
def cube(n: Int): Int = n * n * n
applyFunction(3, double)
applyFunction(3, square)
applyFunction(3, cube)
// anonymous functions
// full deklarasjon
applyFunction(4, (x: Int) => {-x})

// enklere
applyFunction(4, x => -x)

// enklest nja men kortest.
applyFunction(4, -_)

//composisjon av funksjoner.
val composed = double _ compose cube compose square
// er samme som
def equaledToComposed(x: Int) = double(cube(square(x)))
// kommer tilbake med mer spennende komponering i en senere WS
composed(3)
equaledToComposed(3)
// funksjoner i data strukturer
val functions = List(double _, square _ , cube _)
val listComposed = functions reduce(_ compose _)
listComposed(3)
