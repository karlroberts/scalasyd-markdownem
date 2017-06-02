package parser

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}


/**
  * A parser is a function from an input string, to a parse result.
  */
case class Parser[A](run: String => ParseState[A]) {
  /**
    * Return a parser with the function `f` applied to the
    * output of that parser.
    */
  def map[B](f: A => B): Parser[B] =
    Parser(input => run(input).map(f))

  /**
    * Return a parser that feeds its input into this parser, and
    *
    * - if that parser succeeds, apply its result to function f, and
    * then run the resultant parser with the updated input
    *
    * - if that parser fails with an error, return a parser with
    * that error
    */
  def flatMap[B](f: A => Parser[B]): Parser[B] =

    Parser(input => run(input) match {
      case ParseOk(rest, a) =>
        f(a).run(rest)
      case ParseKo(message) =>
        ParseKo(message)
    })

  /**
    * Anonymous flatMap.
    *
    * Return a parser that feeds its input into this parser, and
    *
    * - if that parser succeeds, run the next parser with the updated input
    *
    * - if that parser fails with an error, return a parser with that error
    */
  def >>>[B](parser: => Parser[B]): Parser[B] =
    flatMap(_ => parser)


  /**
    * combinator, both parsers must succeed, but ignore the result of the first.
    * This effectivly nibbles off the input that the first consumes before giving it to the second parser
    */
  def ~>[B](parser: => Parser[B]): Parser[B] = for {
    _ <- this
    r <- parser
  } yield(r)

  /**
    * combinator, both parsers must succeed, but ignore the result of the second.
    * This effectivly nibbles off the input that the second consumes after having parsed with the first.
    */
  def <~[B](parser: => Parser[B]): Parser[A] = for {
    r <- this
    _ <- parser
  } yield(r)

  /**
    * Choice.
    *
    * Return a parser that tries the first parser for a successful value.
    *
    *  - if the first parser succeeds then use this parser
    *
    *  - if the second parser succeeds then try the second parser
    */
  def |||(f: => Parser[A]): Parser[A] =
    Parser(input => run(input) match {
      case ParseOk(rest, a) =>
        ParseOk(rest, a)
      case ParseKo(_) =>
        f.run(input)
    })

  /** run a parser that generates a String then use that String as my own input.
    * Useful when a preexisting parser can be used to parse a sub expression
    * @param parser
    * @return
    */
  def use(parser: Parser[String]): Parser[A]  =
    Parser(input => parser.run(input) match {
      case ParseOk(rest, a) => this.run(a) match {
        case ParseOk(i, v) => ParseOk(rest,v)
        case ParseKo(m) => ParseKo(m)
      }
      case ParseKo(m) => ParseKo(m)
    })

  def usemap[B](parser: Parser[B])(f: B => String) = use(parser map f)

  /**
    * Alias for usemap
    */
  def <<=[B](parser: Parser[B])(f: B => String): Parser[A] = usemap(parser)(f)

}

object Parser {
  /**
    * Return a parser that always succeeds with the given value
    * and consumes no input.
    */
  def value[A](a: A): Parser[A] =
    Parser(input => ParseState.ok(input, a))

  /**
    * Return a parser that always fails with the given message.
    */
  def failed[A](message: String): Parser[A] =
    Parser(_ => ParseState.ko(message))

  /**
    * Return a parser that succeeds with a character off the input
    * or fails if the input is empty.
    */
  def character: Parser[Char] =
    Parser(input => input.toList match {
      case Nil =>
        ParseState.ko("Not enough input to consume a character.")
      case h :: t =>
        ParseState.ok(t.mkString, h)
    })

  /**
    * Return a parser that continues producing a list of values from the
    * given parser.
    */
  def list[A](parser: Parser[A]): Parser[List[A]] =
    list1(parser) ||| value(Nil)

  /**
    * Return a parser that produces at least one value from the
    * given parser then continues producing a list of values from
    * the given parser (to ultimately produce a non-empty list).
    */
  def list1[A](parser: Parser[A]): Parser[List[A]] = for {
    h <- parser
    t <- list(parser)
  } yield h :: t

  /**
    * Return a parser that produces a character but fails if
    *
    *  - The input is empty, or
    *
    *  - The character does not satisfy the given predicate
    */
  def satisfy(pred: Char => Boolean): Parser[Char] =
    character.flatMap(c =>
      if (pred(c))
        value(c)
      else
        failed(s"Input failed to match predicate.: char= $c  pred= $pred"))


  /**
    * Return a parser that produces a character but fails if
    *
    *  - The input is empty, or
    *
    *  - The character does not match the given character
    */
  def is(char: Char): Parser[Char] =
    satisfy(_ == char)

  def isNot(char: Char): Parser[Char] =
    satisfy(_ != char)

  /**
    * return a parser that prodces a character if the char matches a char in the chars list
    * @param chars
    * @return
    */
  def isIn(chars: List[Char]): Parser[Char] =
    satisfy(chars.toSet(_))


  def isNotIn(chars: List[Char]): Parser[Char] =
    satisfy(!chars.toSet(_))


  /**
    * Return a parser that produces a character between '0' and '9'
    * but fails if
    *
    *  - The input is empty, or
    *
    *  - The produced character is not a digit
    */
  def digit: Parser[Char] =
    satisfy(_.isDigit)

  /**
    * Return a parser that produces zero or a positive integer but fails if
    *
    *  - The input is empty, or
    *
    *  - The input does not produce a value series of digits
    */
  def natural: Parser[Int] =
    list1(digit).flatMap(digits =>
      Try(digits.mkString.toInt) match {
        case Success(n) =>
          value(n)
        case Failure(e) =>
          failed(e.toString)
      })

  /**
    * Return a parser that produces a space character but fails if
    *
    *  - The input is empty, or
    *
    *  - The produced character is not a space
    */
  def space: Parser[Char] =
    is(' ')

  /**
    * Return a parse that produces one of more space characters
    * (consuming until the first non-space) but fails if
    *
    *  - The input is empty, or
    *
    *  - The first produced character is not a space
    */
  def spaces1: Parser[String] =
    list1(space).map(_.mkString)

  /**
    * Return a parser that produces a lower-case character but fails if
    *
    *  - The input is empty, or
    *
    *  - The first produced character is not lower-case
    */
  def lower: Parser[Char] =
    satisfy(_.isLower)

  /**
    * Return a parser that produces an upper-case character but fails if
    *
    *  - The input is empty, or
    *
    *  - The first produced character is not upper-case
    */
  def upper: Parser[Char] =
    satisfy(_.isUpper)

  /**
    * Return a parser that produces an alpha character but fails if
    *
    *  - The input is empty, or
    *
    *  - The first produced character is not alpha
    */
  def alpha: Parser[Char] =
    satisfy(_.isLetter)

  def whitespace: Parser[Char] = satisfy(_.isWhitespace)
  def visible: Parser[Char] = satisfy(! _.isWhitespace)

  def word: Parser[String] = list1(visible) map (_.mkString(""))

  def keyword(wordToFind: String): Parser[String] = {
    Parser(input => {
      val parsed: ParseState[String] = word.run(input)
      parsed match {
        case ParseKo(m) => ParseKo(s"Not a word, expected $wordToFind, error message: $m")
        case ParseOk(i, v) => if(wordToFind == v) ParseOk(i,v) else ParseKo(s"invalid keyword match, expected $wordToFind, actual: $v")
      }
    })
  }

  def notKeyword(wordToNotFind: String): Parser[String] = {
    Parser(input => {
      val parsed: ParseState[String] = word.run(input)
      parsed match {
        case ParseKo(m) => ParseKo(s"Not a word, expected $wordToNotFind, error message: $m")
        case ParseOk(i, v) => if(wordToNotFind != v) ParseOk(i,v) else ParseKo(s"Oh no we matched keyword match, expected to find $wordToNotFind, actual: $v")
      }
    })
  }

  def opt[A](parser: Parser[A]): Parser[Option[A]] = {
    Parser(input => {
      val parsed: ParseState[A] = parser.run(input)
      parsed match {
        case ParseKo(m) => ParseOk(input, None)
        case ParseOk(i, v) => ParseOk(i, Some(v))
      }
    })
  }




  def runOn[A](parser: Parser[A], data: List[String]): Either[String, List[A]] =
    ParseState.sequence(data.map(parser.run)) match {
      case ParseKo(message) =>
        Left(message)
      case ParseOk(_, a) =>
        Right(a)
    }

  /**
    * parse and if OK and input remaining keep parsing
    * @param parser
    * @param data
    * @tparam A
    */
  def parseAll[A](parser: Parser[A], data: String): Either[String, List[A]] = {

    @tailrec
    def loop(data: String, ret: Either[String, List[A]]): Either[String, List[A]] = {
      (data, ret) match {
        case ("", _) => ret
        case(_, Left(m)) => ret
        case(d, Right(as)) =>
          parser.run (d) match {
            case ParseKo (oops) => /*println(s"oops wtf? data '$data'  :: ret = $ret");*/ loop (data,Left (oops))
            case ParseOk (i, a) => /*println(s"hehhehe  data '$data'  :: ret = $ret"); */loop (i, ret.flatMap (as => Right (as :+ a) ) )
          }
      }
    }
    loop(data, Right(Nil))
  }


}

case class ParseOk[A](input: String, value: A) extends ParseState[A]
case class ParseKo[A](message: String) extends ParseState[A]

/**
  * Created by robertk on 13/05/17.
  */
sealed trait ParseState[A] {
  def map[B](f: A => B): ParseState[B] =

    this match {
      case ParseOk (input, value) =>
        ParseOk (input, f (value) )
      case ParseKo (message) =>
        ParseKo (message)
    }

  def flatMap[B] (f: A => ParseState[B] ): ParseState[B] =
    this match {
      case ParseOk (input, value) =>
        f (value)
      case ParseKo (message) =>
        ParseKo (message)
    }
}

object ParseState {
  def ok[A](input: String, value: A): ParseState[A] =
    ParseOk(input, value)

  def ko[A](message: String): ParseState[A] =
    ParseKo(message)

  def sequence[A](states: List[ParseState[A]]): ParseState[List[A]] =
    states match {
      case Nil =>
        ok[List[A]]("", Nil)
      case sh :: st => for {
        h <- sh
        t <- sequence(st)
      } yield h :: t
    }
}


