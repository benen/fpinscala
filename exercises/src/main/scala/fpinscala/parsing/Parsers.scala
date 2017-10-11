package fpinscala.parsing

import language.higherKinds

trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def listOfN[A](i: Int, parser: Parser[A]): Parser[List[A]]

  def int(i:  Int): Parser[Int]
  def zeroOrMore[A](p: Parser[A]): Parser[Int] = or(oneOrMore(p), int(0))
  def oneOrMore[A](p: Parser[A]): Parser[Int]
  def andThen[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def exists[A](p: Parser[A]): Parser[Boolean]
  def zomOrOom[A](p1: Parser[A], p2: Parser[A]): Parser[(Int, Int)] = andThen(zeroOrMore(p1), )

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  object Laws {

    val s1 = "abra"
    val s2 = "cadabra"

    run(string(s1))(s1) == Right(s1)
    run(or(string(s1), string(s2)))(s1) == Right(s1)
    run(or(string(s1), string(s2)))(s2) == Right(s2)
    run(listOfN(3, or(string(s1), string(s2))))(s1 + s2 + s1) == Right(s1 + s2 + s1)


  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def &[B>:A](p2: Parser[B]): Parser[B] = self.andThen(p, p2)
    def and[B>:A](p2: => Parser[B]): Parser[B] = self.andThen(p, p2)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}