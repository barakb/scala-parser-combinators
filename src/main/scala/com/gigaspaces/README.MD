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

- `def string(s: String): Parser[String]`. 
   - with rule `run(string(s))(s) == Right(s)` 
- `def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]`  
   - with rule `or(string("abra"),string("cadabra"))`
- `def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]` 
   with rules:
    - `run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")` 
    - `run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")` 
    - `run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")`
 
### Possible algebra
- `def many[A](p: Parser[A]): Parser[List[A]]`
- `def map[A,B](a: Parser[A])(f: A => B): Parser[B]`
* implement char in terms of string
* And similarly another combinator, succeed `def succeed[A](a: A): Parser[A]`, can be defined in terms of string and map the rule is
    * `run(succeed(a))(s) == Right(a)`
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
 1. **string(s)** : Recognizes and returns a single String
 2. **regex(s)** : Recognizes a regular expression s
 3. **slice(p)** : Returns the portion of input inspected by p if successful
 4. **succeed(a)** : Always succeeds with the value a
 5. **flatMap(p)(f)** : Runs a parser, then uses its result to select a second parser to run in sequence
 6. **or(p1,p2)** : Chooses between two parsers, first attempting p1 , and then p2 if p1
  fails

### Implementing the parser
 1. Leftover from last lecture add those functions to ParserOps
    * many
    * many1
    * slice
    * map2
    * listOfN
    * flatMap

 1. Add `Location` and `ParseError` case classes to Parsers.scala
 2. Remove the [ParseError] from the parsers

 3. Using `label` to control the error message
    * Rule label(msg)(fail) == Left(e),  errorMessage(e) == msg
 4. Nesting error messages, what error message do we expect from p on this input ?
 
    ````Scala
    val p = label("first magic word")("abra") **
    " ".many **
    label("second magic word")("cadabra")

    run(p)("abra cAdabra")
   
    ````
 5. Add method `scope` to control nested error message.
 6. Update ParseError to hold nested error messages and locations.    
 7. Add the helper method `errorMessage` and `errorLocation`

 8. Controlling branching and backtracking, What ParseError would we like to get back from `run(p)("abra cAdabra")`
 
      ````Scala
      val spaces = " ".many
      val p1 = scope("magic spell") {
         "abra" ** spaces ** "cadabra"
      }
      val p2 = scope("gibberish") {
         "abba" ** spaces ** "babba"
      }
      val p = p1 or p2
      ````

      Parser p1 consumed part of the input and thus we should commit to p1 unless explicit required not to.
 9. Add `def attempt[A](p: Parser[A]): Parser[A]` with the rule `attempt(p flatMap (_ => fail)) or p2 == p2`              
    Example:
       run the input `"abra cadabra!"` on this parser
       
       ````Scala
        (attempt("abra" ** spaces ** "abra") ** "cadabra") 
        or 
        ("abra" ** spaces "cadabra!")
    
  
       ````
 10. create class ParserImpl 
 
     ````Scala
     object ParserTypesImpl{
       type Parser[+A] = String => Either[ParseError,A]
     }
     
     object ParserImpl extends Parsers[Parser]{
     
     }
     ````
 10. define JSON.scala
     Start with this:
     
     ````Scala
     trait JSON
     
     object JSON {
     
       case object JNull extends JSON
     
       case class JNumber(get: Double) extends JSON
     
       case class JString(get: String) extends JSON
     
       case class JBool(get: Boolean) extends JSON
     
       case class JArray(get: IndexedSeq[JSON]) extends JSON
     
       case class JObject(get: Map[String, JSON]) extends JSON
     
       def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
         import P._
         val spaces: Parser[String] = char(' ').many.slice
     
         val p = label("first magic word")("abra") **
           spaces **
           label("second magic word")("cadabra")
     
     
         succeed(JNull) //todo fixme
       }
     }
     ````
     
