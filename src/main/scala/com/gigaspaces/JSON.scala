package com.gigaspaces


/**
  * Created by Barak Bar Orion
  * on 9/27/17.
  */

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

    def whitespace: Parser[String] = "\\s*".r

    val p = label("first magic word")("abra") **
      whitespace **
      label("second magic word")("cadabra")

    def number: Parser[JSON] = doubleString map (s => JNumber(s.toDouble))

    number or succeed(JNull) //todo fixme !
  }
}

object JSONExample extends App {
  val jsonTxt =
    """
      {
        "name1" : "value1",
        "name2" : "value2",
        "active" : true,
        "price" : 30.66,
        "related" : ["YBR", "CBF", 1.3, true, {"g": "f"}]
      }
    """
  val P = ParserImpl
  import ParserTypesImpl.Parser
  val json: Parser[JSON] = JSON.jsonParser(P)
  println(P.run(json)("1.2"))
}

