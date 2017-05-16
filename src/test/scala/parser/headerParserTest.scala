package parser

import org.specs2._
import org.specs2.matcher.MustMatchers

class MarkdownParserSpec extends HeaderTests { def is = s2"""

 This is a specification to check the 'Markdown Header' parser

 The 'headerParser' string should
   parse a line starting with a '#' as a h1                         $e1
   strip any whitespace after the '#'s before the content           $e2
   strip the '#'s from the content prefix                           $e3
   strip the new line char from the end of the content if it exits  $e4
   fail if there is no visible content                              $e5

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
    case ParseOk(i,v: h1) => v must_==(h1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = headerParser.run(h1_ns_in) match {
    case ParseOk(i,v: h1) => v must_==(h1_ns_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4 = headerParser.run(h1_nl_in) match {
    case ParseOk(i,v: h1) => v must_==(h1_nl_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5 = headerParser.run(h1_empty_in) match {
    case ParseOk(i,v: h1) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }
}


trait HeaderFixtures {
  val h1_in = "# This is a simple **h1** line"
  var h1_val = h1(raw("This is a simple **h1** line"))

  val h1_nl_in = "# This is a simple **h1** line with a newline char\n"
  val h1_nl_val = h1(raw("This is a simple **h1** line with a newline char"))

  val h1_ns_in = "#This is a simple **h1** line with no spaces after the # prefix"
  val h1_ns_val = h1(raw("This is a simple **h1** line with no spaces after the # prefix"))

  val h1_empty_in = "#          \n"

}
