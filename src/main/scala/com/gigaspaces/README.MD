### Parser combinators


### Goal to parse string to JSON
We will represent JSON structure using:

````Scala

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
````


### Parser, Char and run
* def char(c: Char): Parser[Char]
* def run[A](p: Parser[A])(input: String): Either[ParseError,A]
* rule: `run(char(c))(c.toString) == Right(c)`


### Implement combinators first

````Scala
trait Parsers[ParseError, Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char]
  ///Parser is a type parameter
}
````

### Adding infix syntax to parsers

````Scala
trait Parsers[ParseError, Parser[+ _]] {
  self =>
  //  run(char(c))(c.toString) == Right(c)
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  //  run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  
  //  run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String]
  
  case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}

````

### Adding 

- `def string(s: String): Parser[String]` with rule `run(string(s))(s) == Right(s)`
- `def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]` with rule `or(string("abra"),string("cadabra"))`
- `def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]` with rules `run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")` `run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")` `run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")`
 
### Possible algebra
- `def many[A](p: Parser[A]): Parser[List[A]]`
- `def map[A,B](a: Parser[A])(f: A => B): Parser[B]`
* implement char in terms of string
* And similarly another combinator, succeed `def succeed[A](a: A): Parser[A]`, can be defined in terms of string and map the rule is `run(succeed(a))(s) == Right(a)`
- `def slice[A](p: Parser[A]): Parser[String]`
- `def many1[A](p: Parser[A]): Parser[List[A]]`
- `def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]` 
* add `**` and `product` ad infix ops
* Use product to implement the combinator map2 `def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C]`
* Define many in terms of `or` , `map2` , and `succeed`
* Using `map2` and `succeed` , implement the `listOfN`

### Context sensitive 
Suppose we want to parse a single digit, like '4' , followed by that many 'a' characters (this sort of problem should feel familiar from previous chapters).
Examples of valid input are "0" , "1a" , "2aa" , "4aaaa" , and so on. This is an example of a context sensitive grammar.
It can’t be expressed with product because our choice of the second parser depends on the result of the first (the second parser depends on its context).

- `def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]`
- `implicit def regex(r: Regex): Parser[String]`

* Implement `product` and `map2` in terms of `flatMap`.
* Map is no longer primitive. Express it in terms of flatMap and/or other combinators.



### Primitives 
 1. string(s) : Recognizes and returns a single String
 2. regex(s) : Recognizes a regular expression s
 3. slice(p) : Returns the portion of input inspected by p if successful
 4. succeed(a) : Always succeeds with the value a
 5. flatMap(p)(f) : Runs a parser, then uses its result to select a second parser to run in sequence
 6. or(p1,p2) : Chooses between two parsers, first attempting p1 , and then p2 if p1
  fails
