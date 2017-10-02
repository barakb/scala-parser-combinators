package com.gigaspaces

import com.gigaspaces.ParserTypesImpl._

import scala.util.matching.Regex

/**
  * Created by Barak Bar Orion
  * on 9/27/17.
  */

object ParserTypesImpl {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {

    def extract: Either[ParseError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommited = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, consumed: Int) extends Result[A]

  case class Failure(error: ParseError, isCommited: Boolean) extends Result[Nothing]


}

object ParserImpl extends Parsers[Parser] {

  override implicit def string(s: String): Parser[String] =
    location => if (location.input.startsWith(s))
      Success(s, s.length)
    else
      Failure(location.toError("Failed to parse string: " + s), isCommited = false)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)).extract

  override implicit def regex(r: Regex): Parser[String] =
    location => r.findPrefixOf(location.input) match {
      case Some(s) => Success(s, s.length)
      case None => Failure(location.toError("Failed to parse regexp: " + r), isCommited = false)
    }

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    location => p1(location) match {
      case Failure(_, false) => p2(location)
      case a@_ => a
    }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] =
    location => p(location) match {
      case Success(get, consumed) => f(get)(location.advanceBy(consumed)).addCommit(consumed != 0).advanceSuccess(consumed)
      case f@Failure(_, _) => f
    }

  override def slice[A](p: Parser[A]): Parser[String] =
    location => p(location) match {
      case Success(_, consumed) => Success(location.input.substring(0, consumed), consumed)
      case f@Failure(_, _) => f
    }


  override def label[A](msg: String)(p: Parser[A]): Parser[A] = location => p(location).mapError(_.push(location, msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = location => p(location).mapError(_.push(location, msg))

  override def attempt[A](p: Parser[A]): Parser[A] = location => p(location).uncommit

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def fail[A]: Parser[A] = location => Failure(location.toError("fail"), isCommited = false)
}