package parser

import ast._
import org.specs2.Specification
import org.specs2.matcher.MustMatchers

class UrlParserSpec extends UrlTests { def is = s2"""

 This is a specification to check the 'Markdown url' parser

 The 'urlParser' string should
   parse a simple URL of shape [text](link)                         $e1
   parse a simple URL of shape [text](link title)                   $e2
   parse a URL with markdown as test of shape [inlinemarkdown](link title)                   $e3
   fail if there is no visible content                              $e5

 The 'refLinkParser' should
   parse a simple reflink of the form [linktext][ref key]           $e6
   parse a simple reflink of the form [inlinemarkdown][ref key]     $e7
                                                                 """


}

trait UrlTests extends Specification with MustMatchers with UrlFixtures {
  import markdownParser._
  def e1 = linkParser.run(u1_in)  match {
    case ParseOk(i,v) => v must_==(u1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e2 = linkParser.run(u2_in)  match {
    case ParseOk(i,v) => v must_==(u2_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = {
    val r1 = linkParser.run(u3_in) match {
      case ParseOk(i, v) => v must_== (u3_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }

    r1
  }

  def e5 = linkParser.run(u1_empty_in) match {
    case ParseOk(i,v: H1) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }

  def e6 = {
    val r1 = refLinkParser.run(rl1_in) match {
    case ParseOk(i, v) => v must_== (rl1_val)
    case ParseKo(oops) => oops must_== ("fail")
    case default => "oops" must_== ("fail")
    }
    r1
  }

  def e7 = {
    val r1 = refLinkParser.run(rl2_in) match {
      case ParseOk(i, v) => v must_== (rl2_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1
  }

}

trait UrlFixtures {
  val u1_in = "[simple url txt](http://foo.bar.com)"
  val u1_val = Link(rawHtml("simple url txt"), Uri("http://foo.bar.com"), None)

  val u2_in = "[simple url txt](http://foo.bar.com mytitle)"
  val u2_val = Link(rawHtml("simple url txt"), Uri("http://foo.bar.com"), Some("mytitle"))

  val u3_in = "[**OMG this is Bold**](http://foo.bar.com)"
  val u3_val = Link(Bold(List(rawHtml("OMG this is Bold"))), Uri("http://foo.bar.com"), None)

  val u4_in = "[__OMG this is Bold__](http://foo.bar.com)"
  val u4_val = Link(Bold(List(rawHtml("OMG this is Bold"))), Uri("http://foo.bar.com"), None)

  val rl1_in = "[simple ref link text][case insensitive ref link key]"
  val rl1_val = RefLink(rawHtml("simple ref link text"), "case insensitive ref link key")

  val rl2_in = "[~~OMG this is crossed out~~][case insensitive ref link key]"
  val rl2_val = RefLink(Strikethrough(List(rawHtml("OMG this is crossed out"))), "case insensitive ref link key")

  val u1_empty_in = ""
}