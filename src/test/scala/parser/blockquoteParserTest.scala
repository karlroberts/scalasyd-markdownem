package parser

import ast.{Blockquote, RawHtml, Bold, Br}
import org.specs2._
import org.specs2.matcher.MustMatchers

class BlockquoteParserSpec extends BQTests { def is = s2"""

 This is a specification to check the 'Markdown BlockQuote' parser

 The 'blockParser' string should
   parse a line starting with a '>' as a blockquote                 $e1
   strip any whitespace after the '>'s before the content           $e2
   strip the '>'s from the content prefix                           $e3
   strip the new line char from the end of the content if it exits  $e4
   fail if there is no visible content                              $e5
   join up multiple consecutive lines of blockquotes into one blockquote of multiple lines $e6

                                                                 """


}

trait BQTests extends Specification with MustMatchers with BQFixtures {
  import markdownParser._

  def e1 = blockquoteParser.run(h1_in) match {
    case ParseOk(i,v) => v must_==(h1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e2 = blockquoteParser.run(h1_in) match {
    case ParseOk(i,v: Blockquote) => v must_==(h1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = blockquoteParser.run(h1_ns_in) match {
    case ParseOk(i,v) => v must_==(h1_ns_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4 = blockquoteParser.run(h1_nl_in) match {
    case ParseOk(i,v) => v must_==(h1_nl_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5 = blockquoteParser.run(h1_empty_in) match {
    case ParseOk(i,v) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }

  def e6 = blockquoteParser.run(bq_multiline_in) match {
    case ParseOk(i,v) => v must_==(bq_multiline_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }
}


trait BQFixtures {
  val h1_in = "> This is a simple **bq** line"
  var h1_val = Blockquote(List(RawHtml("This is a simple "), Bold(List(RawHtml("bq"))),RawHtml(" line")))

  val h1_nl_in = "> This is a simple **bq** line with a newline char\n"
  val h1_nl_val = Blockquote(List(RawHtml("This is a simple "), Bold(List(RawHtml("bq"))), RawHtml(" line with a newline char")))

  val h1_ns_in = ">This is a simple **bq** line with no spaces after the > prefix"
  val h1_ns_val = Blockquote(List(RawHtml("This is a simple "),Bold(List(RawHtml("bq"))),RawHtml(" line with no spaces after the > prefix")))

  val h1_newline_only_in = "#          \n"
  val h1_newline_only_val = Blockquote(List(Br))

  val h1_empty_in = ">                "

  val bq_multiline_in =
    """>simple bq line
      |> another bq line
      |> foofofof""".stripMargin
  val bq_multiline_val = Blockquote(List(RawHtml("simple bq line"),Br,RawHtml("another bq line"), Br, RawHtml("foofofof")))

}
