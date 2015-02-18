
val userAndPassword = ("alfred", "secret")
val organizationKey = "abd4kgo3wgbo2"

val login = userAndPassword



def check(s: String, s1: String) = true

object api {
  def check(tuple: (String, String)) = true
}

login match {
  case (username, password) => check(username, password)
  case authKey => api.check(authKey)
  case _ => false
}



val a = List("a", "b", "c")
val res = a match {
  case Nil => "list to short"
  case _ :: Nil => "list to short"
  case _ :: second :: _ => second
  case _ => "list to big"
}


case class Address(street: String, country: String)

case class Person(u: String, p: String, a: Address)

val countrySupported = Person("name", "password", Address("street", "no"))

def isCountrySupported(country: String) = country == "no"

countrySupported match {
  case Person(_, _, Address(_, country)) => {
    isCountrySupported(country)
  }
  case _ => false
}


val someval:Any = ("someval", "anotherval")

someval match {
  case i: Int => "an int"
  case s: String => "a string"
  case (s1: String, s2: String) => "2 strings"
  case _ => "wtf"
}

