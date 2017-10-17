package com.gigaspaces

import scala.util.matching.Regex


/**
  * Created by Barak Bar Orion
  * on 9/18/17.
  */


trait Parsers[Parser[+ _]] {
  parsers =>
  // run(char(c))(c.toString) == Right(c)
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = (p product p2) map f.tupled

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List())
    case _ => map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(x => succeed(f(x)))

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))


  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A, B >: A](p1: Parser[A], p2: => Parser[B]): Parser[B]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]


  implicit def toParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = ParserOps(f(a))

  implicit def toParserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)


  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  // (attempt(succeed(1) flatMap (_ => fail)) : Parser[Int]) or succeed(2) == succeed(2)

  // for all s. run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A]

  def fail[A]: Parser[A]

  def token[A](p: Parser[A]): Parser[A] = (p ** string(" ").many).map(_._1)

  def doubleString: Parser[String] = {
    token(regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)).slice
  }


  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = parsers.or(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = parsers.or(p, p2)

    def map[B](f: A => B): Parser[B] = parsers.map(p)(f)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = parsers.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = parsers.product(p, p2)

    // --------------------------------------------------------------

    def many: Parser[List[A]] = parsers.many(p)

    def many1: Parser[List[A]] = parsers.many1(p)

    def slice: Parser[String] = parsers.slice(p)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = parsers.map2(p, p2)(f)

    def listOfN(n: Int): Parser[List[A]] = parsers.listOfN(n, p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = parsers.flatMap(p)(f)

    def label(msg: String): Parser[A] = parsers.label(msg)(p)

    def scope(msg: String): Parser[A] = parsers.scope(msg)(p)
  }

}

case class Location(in: String, offset: Int = 0) {
  lazy val line: Int = in.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = in.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (in.length > 1) in.lines.drop(line - 1).next
    else ""

  def input: String = in.substring(offset)

}

case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  override def toString: String = ??? //home work
}







