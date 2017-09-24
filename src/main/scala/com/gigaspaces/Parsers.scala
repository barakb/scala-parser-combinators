package com.gigaspaces

import scala.util.matching.Regex


/**
  * Created by Barak Bar Orion
  * on 9/18/17.
  */


trait Parsers[ParseError, Parser[+_]]{
  self =>
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] // ------------------
  // run(char(c))(c.toString) == Right(c)


  //  val p: Parser[Char] = or(char('c'), char('b'))
  //  run(p)("b") == Right('b')
  //  run(p)("c") == Right('c')
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] // ------------------
//  val p: Parser[Char] = 'c' | char('b')

  // run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String] // ------------------
//  val p: Parser[String] = string("foo")

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List())
    case _ => map2(p, listOfN(n - 1, p))(_::_)
  }
  // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")

//  or map2 succeed
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_) or succeed(List())

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(a => succeed(f(a)))

  // run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String] // ------------------

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_)

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = map2(p, p2)((_,_))

//  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = p ** p2 map f.tupled
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = flatMap(p)(a => map(p2)(b => f(a, b)))

  implicit def regex(r: Regex): Parser[String] // ------------------

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] // ------------------

  implicit def toParserOps[A](p: Parser[A]): ParserOps[A] = new ParserOps[A](p)
  implicit def toParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f : A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f : A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)


  }

  "1,2,4"
  regex("[0-9]".r)
  char(',')
  map2(regex("[0-9]".r), string(","))(_ + _)
  "number,"
  product(many(map2(regex("[0-9]".r), string(","))(_ + _)),regex("[0-9]".r)) | string("")

  "na" // the number of the a's is n

  "0"
  "1a"
  "2aa"
  "3aaa"

}

