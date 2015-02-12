//funksjonen

// Navn på verdien etterfulgt av Type.
val i: Int = 1
val i1 = 1 // renger ikke her dav 1 opplagt er Int

val

def max(x: Int, y: Int): Int = {
  if (x > y) x
  else y
}

max(2, 4)
//trenger ikke spesifisere typer hvis det er gitt.
//er er det opplagt at vi retunerer en Int
def max2(x: Int, y: Int) = {
  if (x > y) x
  else y
}
//hvis funksjonen kun er et statement kan man droppe { }
def max3(x: Int, y: Int) = if (x > y) x else y
max3(2, 4)
//en funksjon kan ha funksjoner
def factorial(n: Int): Int = {
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n - 1, n * acc)

  go(n, 1)
}

factorial(3)
//kan returnere funjsoner
def functionFactory(n: Int): () => Int = {
  def identithy(): Int = n

  identithy
}

functionFactory(3) // er en funksjone
