package parser

import java.net.URL

/**
  * AST as an ADT
  */
sealed trait Markdown


// an escape character
case class escape(value: Char) extends Markdown

//if we have raw HTML we just output it, no need to parse it for content
case class rawHtml(value: Html) extends Markdown

//just dumped a sting into here, dont know yet if it contains any markdown so inline needs to be parsed.
case class raw(value: String) extends Markdown // do i need this for content? or just use rawHtml?

// h1 header the value in the header could be more markdown
case class h1(value: Markdown) extends Markdown
case class h2(value: Markdown) extends Markdown
case class h3(value: Markdown) extends Markdown
case class h4(value: Markdown) extends Markdown
case class h5(value: Markdown) extends Markdown
case class h6(value: Markdown) extends Markdown
case class h7(value: Markdown) extends Markdown

//a new line is a new <p></p> block i think
case class paragraph(value: Markdown) extends Markdown

//Emphasid
case class bold(value: Markdown) extends Markdown
case class italic(value: Markdown) extends Markdown

//Blockquote
case class blockquote(value: Markdown) extends Markdown

//Lists
case class li(value: Markdown) extends Markdown
case class ul(items: List[li]) extends Markdown
case class ol(items: List[li]) extends Markdown

//Images
//case class img(alt: String, url: URL)

//Links
case class link(linkText: Markdown, url: URL)


/**
  * Parse Markdown into a parse tree
  * Created by robertk on 13/05/17.
  */
object markdownParser {
  import Parser._

//  val markdownPageParser: Parser[List[Markdown]] = ???

  def markdownParser: Parser[Markdown] =
    headerParser ||| blockquoteParser

  val headerParser: Parser[Markdown] = for {
      _ <- list(space)
      hashes <- list1(is('#'))
      _ <- list(space)
      md <- list1(isNot('\n'))
      _ <- (is('\n') ||| value('\n')) // consume a new line or just succeed, we throw away last parse anyway
    } yield hashes.size match {
      case 1       => h1(raw(md.mkString))
      case 2       => h2(raw(md.mkString))
      case 3       => h3(raw(md.mkString))
      case 4       => h4(raw(md.mkString))
      case 5       => h5(raw(md.mkString))
      case 6       => h6(raw(md.mkString))
      case default => h6(raw(md.mkString)) // too many #'s so settle on 6
    }


  val blockquoteParser : Parser[Markdown] = for {
    _ <- list(space)
    tag <- is('>')
    _ <- list(space)
    md <- list1(isNot('\n'))
    _ <- (is('\n') ||| value('\n')) // consume a new line or just succeed, we throw away last parse anyway
  } yield blockquote(raw(md.mkString))



}
