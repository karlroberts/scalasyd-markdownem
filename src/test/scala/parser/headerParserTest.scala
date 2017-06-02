package parser

import ast._
import org.specs2._
import org.specs2.matcher.MustMatchers

class HeaderParserSpec extends HeaderTests { def is = s2"""

 This is a specification to check the 'Markdown Header' parser

 The 'headerParser' string should
   parse a line starting with a '#' as a h1                         $e1
   strip any whitespace after the '#'s before the content           $e2
   strip the '#'s from the content prefix                           $e3
   replave acnewline inline with a Br if it exits                   $e4
   even if it is the only thing inline                              $e5
   Fail if ther is no content in the header                         $e6
   parse a line starting with a '##' as a h2                        $e7
   parse a line starting with a '###' as a h3                        $e8
   parse a line starting with a '####' as a h4                        $e9
   parse a line starting with a '#####' as a h5                        $e10
   parse a line starting with a '######' as a h6                        $e11
   parse a line starting with a '#######' as a h7                        $e12

                                                                 """

}

trait HeaderTests extends Specification with MustMatchers with HeaderFixtures {
  import markdownParser._

  def e1 = headerParser.run(h1_in) match {
    case ParseOk(i,v) => v must_==(h1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e2 = headerParser.run(h1_in) match {
    case ParseOk(i,v: H1) => v must_==(h1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = headerParser.run(h1_ns_in) match {
    case ParseOk(i,v: H1) => v must_==(h1_ns_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4 = headerParser.run(h1_nl_in) match {
    case ParseOk(i,v: H1) => v must_==(h1_nl_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5 = headerParser.run(h1_newline_only_in) match {
    case ParseOk(i,v: H1) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }

  def e6 = headerParser.run(h1_empty_in) match {
    case ParseOk(i,v: H1) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }

  def e7 = headerParser.run(h2_in) match {
    case ParseOk(i,v) => v must_==(h2_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e8 = headerParser.run(h3_in) match {
    case ParseOk(i,v) => v must_==(h3_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e9 = headerParser.run(h4_in) match {
    case ParseOk(i,v) => v must_==(h4_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e10 = headerParser.run(h5_in) match {
    case ParseOk(i,v) => v must_==(h5_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e11 = headerParser.run(h6_in) match {
    case ParseOk(i,v) => v must_==(h6_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e12 = headerParser.run(h7_in) match {
    case ParseOk(i,v) => v must_==(h7_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }
}


trait HeaderFixtures {

  val h1_in = "# This is a simple **h1** line"
  var h1_val = H1(List(rawHtml("This is a simple "),Bold(List(rawHtml("h1"))), rawHtml(" line")))

  val h1_nl_in = "# This is a simple **h1** line with a newline char\n"
  val h1_nl_val = H1(List(rawHtml("This is a simple "),Bold(List(rawHtml("h1"))), rawHtml(" line with a newline char")))

  val h1_ns_in = "#This is a simple **h1** line with no spaces after the # prefix"
  val h1_ns_val = H1(List(rawHtml("This is a simple "),Bold(List(rawHtml("h1"))), rawHtml(" line with no spaces after the # prefix")))

  val h1_newline_only_in = "#          \n"
  val h1_newline_only_val = H1(List(Br))

  val h1_empty_in = "#                "

  val h2_in = "## This is a simple **h2** line"
  var h2_val = H2(List(rawHtml("This is a simple "),Bold(List(rawHtml("h2"))), rawHtml(" line")))

  val h3_in = "### This is a simple **h3** line"
  var h3_val = H3(List(rawHtml("This is a simple "),Bold(List(rawHtml("h3"))), rawHtml(" line")))

  val h4_in = "#### This is a simple **h4** line"
  var h4_val = H4(List(rawHtml("This is a simple "),Bold(List(rawHtml("h4"))), rawHtml(" line")))

  val h5_in = "##### This is a simple **h5** line"
  var h5_val = H5(List(rawHtml("This is a simple "),Bold(List(rawHtml("h5"))), rawHtml(" line")))

  val h6_in = "###### This is a simple **h6** line"
  var h6_val = H6(List(rawHtml("This is a simple "),Bold(List(rawHtml("h6"))), rawHtml(" line")))

  val h7_in = "####### This is a simple **h7** line"
  var h7_val = H7(List(rawHtml("This is a simple "),Bold(List(rawHtml("h7"))), rawHtml(" line")))

}
