package com.gigaspaces

/**
  * Created by Barak Bar Orion
  * on 11/12/17.
  */
//noinspection SpellCheckingInspection
trait Sexpressions
/*
sexpr: atom
    | list

list: '(' sexpr* ')'

atom: ID
    | DOUBLE
    | INTEGER
    | STRING
    ;
 */

//noinspection SpellCheckingInspection
object Sexpressions{

  case class SID(get: String) extends Sexpressions

  case class SString(get: String) extends Sexpressions

  case class SBool(get: Boolean) extends Sexpressions

  case class SDouble(get: Double) extends Sexpressions

  case class SList(get: List[Sexpressions]) extends Sexpressions

  def sexpParser[Parser[+ _]](P: Parsers[Parser]): Parser[Sexpressions] = {
    import P._

    def whitespace: Parser[String] = "\\s*".r

    def sDouble: Parser[Sexpressions] = doubleString map (s => SDouble(s.toDouble))
    def sString: Parser[Sexpressions] = surround(string("\""), string("\""))(regex("[^\"]*".r).slice) map SString
    def sBool: Parser[Sexpressions] = string("#t") map(_ => SBool(true)) or string("#f") map(_ => SBool(false))
    def sId: Parser[Sexpressions] = regex("[a-zA-Z\\+\\-\\*\\=][^\\s\\)\\(]*".r).slice map SID

    def sList: Parser[Sexpressions] = (string("(") *>  sep(value, whitespace) <* string(")")) map SList

    def value: Parser[Sexpressions] =  sDouble or sString or sBool or sId or sList

    value
  }
}

object SexpressionsExample extends App {
  val sexpressionsTxt =
    """(a "b" 1
      | (lambda(x)(+ 2 x))
      | 1.4 )""".stripMargin
  val P = ParserImpl
  import ParserTypesImpl.Parser
  val sexp: Parser[Sexpressions] = Sexpressions.sexpParser(P)
  println(P.run(sexp)("1.2"))
  println(P.run(sexp)("1"))
  println(P.run(sexp)("\"a string\""))
  println(P.run(sexp)("#t"))
  println(P.run(sexp)("#f"))
  println(P.run(sexp)("foo"))
  println(P.run(sexp)("()"))
  println(P.run(sexp)("(lambda(x)(+ 1 x))"))
  println(P.run(sexp)(sexpressionsTxt))
}
