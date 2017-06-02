package parser

import ast._
import org.specs2._
import org.specs2.matcher.MustMatchers

class EmphasisParserSpec extends EmphasisTests { def is = s2"""

 This is a specification to check the 'Markdown bold ad italic Header' parser

 The 'BoldParser' string should
   parse a sentence surrounded with ** as bold                                      $e1
   parse a sentence surrounded with ** as bold with nested inline too               $e1_a
   parse a sentence surrounded with __ as bold                                      $e2
   parse a sentence surrounded with __ as bold with nested inline too               $e2_b
   parse a sentence surrounded with * as italic                                     $e3
   parse a sentence surrounded with * as italic with nested inline bold1 too        $e3_b
   parse a sentence surrounded with _ as italic                                     $e4
   parse a sentence surrounded with _ as italic with nested inline bold2            $e4_b
   parse a sentence surrounded with ~~ as strikethrough                             $e5
   parse a sentence surrounded with ~~ as strikethrough with nested inline italic   $e5_b
   fail if there is no visible content                                              $e_empty

                                                                 """
/*



 */

}

trait EmphasisTests extends Specification with MustMatchers with BoldFixtures {
  import markdownParser._

  def e1 = boldParser.run(b1_in) match {
    case ParseOk(i,v) => v must_==(b1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e1_a = boldParser.run(b1_b_in) match {
    case ParseOk(i,v) => v must_==(b1_b_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e2 = boldParser.run(b2_in) match {
    case ParseOk(i,v) => v must_==(b2_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e2_b = boldParser.run(b2_b_in) match {
    case ParseOk(i,v) => v must_==(b2_b_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = italicParser.run(i1_in) match {
    case ParseOk(i,v) => v must_==(i1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3_b = italicParser.run(i1_b_in) match {
    case ParseOk(i,v) => v must_==(i1_b_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4 = italicParser.run(i2_in) match {
    case ParseOk(i,v) => v must_==(i2_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4_b = italicParser.run(i2_b_in) match {
    case ParseOk(i,v) => v must_==(i2_b_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5 = strikethroughParser.run(st1_in) match {
    case ParseOk(i,v) => v must_==(st1_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5_b = strikethroughParser.run(st1_b_in) match {
    case ParseOk(i,v) => v must_==(st1_b_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }
//  emphasisParser

  def e_empty = emphasisParser.run(h1_empty_in) match {
    case ParseOk(i,v: H1) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }
}


trait BoldFixtures {
  val b1_in = "**this is bold**"
  var b1_val = Bold(List(rawHtml("this is bold")))

  val b1_b_in = "**this _is_ bold**"
  var b1_b_val = Bold(List(rawHtml("this "), Italic(List(rawHtml("is"))), rawHtml(" bold")))

  val b2_in = "__this is bold__"
  var b2_val = Bold(List(rawHtml("this is bold")))

  val b2_b_in = "__this ~~is~~ bold__"
  var b2_b_val = Bold(List(rawHtml("this "), Strikethrough(List(rawHtml("is"))), rawHtml(" bold")))


  val i1_in = "_this is italic_"
  var i1_val = Italic(List(rawHtml("this is italic")))

  val i1_b_in = "_this **is** italic_"
  var i1_b_val = Italic(List(rawHtml("this "), Bold(List(rawHtml("is"))), rawHtml(" italic")))

  val i2_in = "_this is italic_"
  var i2_val = Italic(List(rawHtml("this is italic")))

  val i2_b_in = "_this __is__ italic_"
  var i2_b_val = Italic(List(rawHtml("this "), Bold(List(rawHtml("is"))), rawHtml(" italic")))

  val st1_in = "~~this is strikthrough~~"
  var st1_val = Strikethrough(List(rawHtml("this is strikthrough")))

  val st1_b_in = "~~this _is_ strikthrough~~"
  var st1_b_val = Strikethrough(List(rawHtml("this "), Italic(List(rawHtml("is"))), rawHtml(" strikthrough")))


  val h1_empty_in = "#          \n"

}
