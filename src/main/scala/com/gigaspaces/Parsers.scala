package com.gigaspaces



/**
  * Created by Barak Bar Orion
  * on 9/18/17.
  */


trait Parsers[ParseError, Parser[+_]]{
  foo =>
  // run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  // for all s. run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = (p product p2) map f.tupled

//  "0"
//  "1a"
//  "2aa"
//  "3aaa"

//  run(many(string("a")))("aaa") == Right(List("a", "a", "a"))
//  run(many(string("a")))("") == Right(List())
  // or , map2 , and succeed
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_) or succeed(List())
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_)
  // Using map2 and succeed , implement the listOfN
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List())
    case _ => map2(p, listOfN(n - 1, p))(_::_)
  }
  //  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  //  run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  //  run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(x => succeed(f(x)))

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = flatMap(p)(a => map(p2)(b => (a, b)))



  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  // run(or(char('c'), char('b')))("b") == Right('b')
  //  run(or(char('c'), char('b')))("c") == Right('c')
  //  run(or(char('c'), char('b')))("d") == Left(ParseError)
//  run(char('c') | char('b'))("b")

  //  run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String]

//  run("foo" | "bar")("foo")

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]


  implicit def toParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = ParserOps(f(a))

  implicit def toParserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[A]){
    def or(p2: => Parser[A]): Parser[A] = foo.or(p, p2)
    def |(p2: => Parser[A]): Parser[A] = foo.or(p, p2)
    def map[B](f: A => B): Parser[B] = foo.map(p)(f)
    def product[B](p2: Parser[B]): Parser[(A,B)] = foo.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = foo.product(p, p2)

  }
}

