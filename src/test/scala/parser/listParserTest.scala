package parser

import ast._
import ast.Li._
import org.specs2._
import org.specs2.matcher.MustMatchers

class UliParserSpec extends UliTests { def is = s2"""

 This is a specification to check the 'Markdown List' parser

 The 'uliParser' string should
   parse a line starting with a '*' or '-' as a `uol` and capture indent level  $e1
   strip any whitespace after the '*' or '-' before the content     $e2
   strip the '*' or '-' from the content prefix                     $e3
   strip the new line char from the end of the content if it exits  $e4
   fail if there is no visible content                              $e5

                                                                 """

  //   join up multiple consecutive lines of blockquotes into one blockquote of multiple lines $e6

}

trait UliTests extends Specification with MustMatchers with UliFixtures {
  import markdownParser._

  def e1 = {
//    val r1 = uLIparser.run(ul1_in) match {
//      case ParseOk(i, v) => v must_== (ul1_val)
//      case ParseKo(oops) => oops must_== ("fail")
//      case default => "oops" must_== ("fail")
//    }
    val r2 = uLIparser.run(ul2_in) match {
      case ParseOk(i, v) => v must_== (ul2_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
//    r1 && r2
    r2
  }

  def e2 = uLIparser.run(ul3_in) match {
    case ParseOk(i,v: Uli) => v must_==(ul3_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = uLIparser.run(ul3_in) match {
    case ParseOk(i,v) => v must_==(ul3_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4 = uLIparser.run(ul_nl_in) match {
    case ParseOk(i,v) => v must_==(ul_nl_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5 = uLIparser.run(ul_empty_in) match {
    case ParseOk(i,v) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }
//
//  def e6 = blockquoteParser.run(bq_multiline_in) match {
//    case ParseOk(i,v) => v must_==(bq_multiline_val)
//    case ParseKo(oops) => oops must_==("fail")
//    case default => "oops" must_==("fail")
//  }
}


trait UliFixtures {


  val ul1_in = "* This is a simple **uli** line"
  var ul1_val = uli(List(rawHtml("This is a simple "), Bold(List(rawHtml("uli"))), rawHtml(" line")), 0)

  val ul2_in = "  - This is a simple **uli ~~strikeme~~** line with more stuff in bold"
  var ul2_val = uli(List(rawHtml("This is a simple "), Bold(List(rawHtml("uli "), Strikethrough(List(rawHtml("strikeme"))) )), rawHtml(" line with more stuff in bold")), 2)

  val ul3_in = "  -            This is a simple **uli** line"
  var ul3_val = uli(List(rawHtml("This is a simple "), Bold(List(rawHtml("uli"))), rawHtml(" line")), 2)

  val ul_nl_in = "* This is a simple **uli** line with a newline char"
  val ul_nl_val = uli(List(rawHtml("This is a simple "), Bold(List(rawHtml("uli"))), rawHtml(" line with a newline char")), 0)


  val ul_empty_in = "-          \n"

}


class OliParserSpec extends OliTests { def is = s2"""

 This is a specification to check the 'Markdown List' parser

 The 'uliParser' string should
   parse a line starting with a digit followed by `.` as  `oli` and capture indent level  $e1
   strip any whitespace after the prefix before the content     $e2
   strip the prefix from the content prefix                     $e3
   strip the new line char from the end of the content if it exits  $e4
   fail if there is no visible content                              $e5

                                                                 """

  //   join up multiple consecutive lines of blockquotes into one blockquote of multiple lines $e6

}

trait OliTests extends Specification with MustMatchers with OliFixtures {
  import markdownParser._

  def e1 = {
    val r1 = oLIparser.run(ol1_in) match {
      case ParseOk(i, v) => v must_== (ol1_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    val r2 = oLIparser.run(ol2_in) match {
      case ParseOk(i, v) => v must_== (ol2_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1 && r2
  }

  def e2 = oLIparser.run(ol3_in) match {
    case ParseOk(i,v: Oli) => v must_==(ol3_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e3 = oLIparser.run(ol3_in) match {
    case ParseOk(i,v) => v must_==(ol3_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e4 = oLIparser.run(ol_nl_in) match {
    case ParseOk(i,v) => v must_==(ol_nl_val)
    case ParseKo(oops) => oops must_==("fail")
    case default => "oops" must_==("fail")
  }

  def e5 = oLIparser.run(ol_empty_in) match {
    case ParseOk(i,v) => v must_==("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_==("fail")
  }
  //
  //  def e6 = blockquoteParser.run(bq_multiline_in) match {
  //    case ParseOk(i,v) => v must_==(bq_multiline_val)
  //    case ParseKo(oops) => oops must_==("fail")
  //    case default => "oops" must_==("fail")
  //  }
}


trait OliFixtures {

  val ol1_in = "1. This is a simple **oli** line"
  var ol1_val = oli(List(rawHtml("This is a simple "), Bold(List(rawHtml("oli"))), rawHtml(" line")), 0)

  val ol2_in = "  1. This is a simple **oli** line"
  var ol2_val = oli(List(rawHtml("This is a simple "), Bold(List(rawHtml("oli"))), rawHtml(" line")), 2)

  val ol3_in = "  3.          This is a simple **oli** line"
  var ol3_val = oli(List(rawHtml("This is a simple "), Bold(List(rawHtml("oli"))), rawHtml(" line")), 2)

  val ol_nl_in = "2. This is a simple **oli** line with a newline char\n"
  val ol_nl_val = oli(List(rawHtml("This is a simple "), Bold(List(rawHtml("oli"))), rawHtml(" line with a newline char")), 0)

  val o1_ns_in = "4.This is a simple **ol** line with no spaces after the 1. prefix"
  val o1_ns_val = oli(List(rawHtml("This is a simple **ol** line with no spaces after the 1. prefix")),0)

  val ol_empty_in = "1.          \n"

}

class liParserSpec extends LiTests { def is = s2"""

 This is a specification to check the 'Markdown List' parser

 The 'liParser' string should
   parse a ordered o unordered list  $e1
   join up multiple consecutive lines of  li's into one mdList $e2
   handle mixed stuff ordered or unordered $e3
   handle nesting $e4
   fail if there is no visible content                              $e5
   handle a list thar has a nested list after a few non nested Leafs $e6

                                                                 """
}


trait LiTests extends Specification with MustMatchers with LiFixtures {

  import markdownParser._

  def e1 = {
    val r1 = liParser.run(l1_in) match {
      case ParseOk(i, v) => v must_== (l1_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    val r2 = liParser.run(l2_in) match {
      case ParseOk(i, v) => v must_== (l2_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }

    r1 && r2
  }

  def e2 = {
    val r1 = liParser.run(l3_in) match {
      case ParseOk(i, v) =>  v must_== (l3_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    val r2 = liParser.run(l4_in) match {
      case ParseOk(i, v) =>  v must_== (l4_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1 && r2
  }

  def e3 = {
    liParser.run(l5_in) match {
      case ParseOk(i, v) =>  v must_== (l5_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }

  }

  def e4 = {
    liParser.run(l6_in) match {
      case ParseOk(i, v) => v must_== (l6_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
  }

  def e5 = liParser.run(ol_empty_in) match {
    case ParseOk(i, v) => v must_== ("fail")
    case ParseKo(oops) => oops.length must be_>(0)
    case default => "oops" must_== ("fail")
  }

  def e6 = {
    liParser.run(l12_in) match {
      case ParseOk(i, v) => v must_== (l12_val)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
  }


}


trait LiFixtures {
  import Tree._

  val l1_in = "* this is unordered"
  val l1_val = List(uli(List(rawHtml("this is unordered")), 0))
  val l1_tree =  MdForest(List( Leaf(Uli(List(rawHtml("this is unordered")), 0), 0) ))

  val l2_in = "1. this is ordered"
  val l2_val = List(oli(List(rawHtml("this is ordered")), 0))
  val l2_tree = MdForest(List( Leaf(Oli(List(rawHtml("this is ordered")), 0), 0) ))


  val l3_in =
    """* multi line uol 1
      |* multi line uol 2
    """.stripMargin

  val l3_val = List(uli(List(rawHtml("multi line uol 1")),0), uli(List(rawHtml("multi line uol 2")),0))
  val l3_tree = MdForest( List( Branch(List( Leaf(Uli(List(rawHtml("multi line uol 1")),0), 0), Leaf(Uli(List(rawHtml("multi line uol 2")),0),0) ), 0) ))

  val l4_in =
  """1. multi line oli 1
    |2. multi line oli 2
  """.stripMargin

  val l4_val = List(oli(List(rawHtml("multi line oli 1")),0), oli(List(rawHtml("multi line oli 2")),0))
  val l4_tree = MdForest( List( Branch(List( Leaf(Oli(List(rawHtml("multi line oli 1")),0), 0), Leaf(Oli(List(rawHtml("multi line oli 2")),0),0) ), 0) ))


  val l5_in =
    """* uli 1
      |2. oli 2
    """.stripMargin

  val l5_val = List(uli(List(rawHtml("uli 1")),0), oli(List(rawHtml("oli 2")),0))
  val l5_tree = MdForest(List(Leaf(Uli(List(rawHtml("uli 1")),0),0), Leaf(Oli(List(rawHtml("oli 2")),0),0)))

  val l6_in =
    """* uli 1
      |  2. oli 2
      |  3. oli 3
      |* uli 2
      |    4. oli 4
      |    5. oli 5
      |* uli 3
    """.stripMargin

  val l6_val = List(uli(List(rawHtml("uli 1")),0), oli(List(rawHtml("oli 2")),2), oli(List(rawHtml("oli 3")),2), uli(List(rawHtml("uli 2")),0), oli(List(rawHtml("oli 4")), 4), oli(List(rawHtml("oli 5")), 4), uli(List(rawHtml("uli 3")),0))
  val l6_tree = List()


  val l7_in =
    """* uli 1
      |  2. oli 2
      |* uli 2
    """.stripMargin

  val l7_tree = MdForest(List( Branch(List( Leaf(Uli(List(rawHtml("uli 1")),0),0),  Branch(List(Leaf(Oli(List(rawHtml("oli 2")),2), 2)),2), Leaf(Uli(List(rawHtml("uli 2")),0), 0) ), 0)  ))

  val l8_in =
    """* uli 1
      |  * uli 2deep 1
      |* uli 2
    """.stripMargin

  val l8_tree = MdForest(List( Branch(List( Leaf(Uli(List(rawHtml("uli 1")),0),0),  Branch(List(Leaf(Uli(List(rawHtml("uli 2deep 1")),2), 2)), 2), Leaf(Uli(List(rawHtml("uli 2")),0),0) ),0) ))

  val l9_in =
    """* uli 1
      |  * uli 2deep 1
      |    1. oli 4deep 1
      |* uli 2
    """.stripMargin

  val l9_tree = MdForest(List( Branch( List(Leaf(Uli(List(rawHtml("uli 1")),0),0),  Branch( List(Leaf(Uli(List(rawHtml("uli 2deep 1")),2),2), Branch(List(Leaf(Oli(List(rawHtml("oli 4deep 1")), 4),4)), 4)),2) , Leaf(Uli(List(rawHtml("uli 2")),0),0) ),0) ))

  val l10_in =
    """* uli 1
      |  * uli 2deep 1
      |    1. oli 4deep 1
      |    2. oli 4deep 2
      |      1. oli 6deep 1
      |* uli 2
    """.stripMargin

  val l10_tree = MdForest(List( Branch( List(Leaf(Uli(List(rawHtml("uli 1")),0), 0),  Branch(List(Leaf(Uli(List(rawHtml("uli 2deep 1")),2),2), Branch(List( Leaf(Oli(List(rawHtml("oli 4deep 1")), 4), 4), Leaf(Oli(List(rawHtml("oli 4deep 2")), 4), 4),  Branch(List(Leaf(Oli(List(rawHtml("oli 6deep 1")),6),6)),6) ),4) ), 2), Leaf(Uli(List(rawHtml("uli 2")),0),0)),0) ))

  val l11_in =
    """* uli 1
      |    * uli 4deep 1
      |  1. oli 2deep 1
      |* uli 2
    """.stripMargin

  val l11_tree = MdForest(List(Branch(List(Leaf(Uli(List(rawHtml("uli 1")),0),0), Branch(List(Leaf(Uli(List(rawHtml("uli 4deep 1")),4),4)),4), Branch(List(Leaf(Oli(List(rawHtml("oli 2deep 1")),2),2)),2), Leaf(Uli(List(rawHtml("uli 2")),0),0)), 0) ))

  val l12_in =
    """1. order 1
      |2. order 2
      |  * nest2Unorderd 4
      |3. order 3
    """.stripMargin
  Leaf(Oli(List(rawHtml("order 2")),0),0)

  val l12_val = List( Oli(List(rawHtml("order 1")), 0) , Oli(List(rawHtml("order 2")), 0) , Uli(List(rawHtml("nest2Unorderd 4")), 2)  , Oli(List(rawHtml("order 3")), 0) )
  val l12_tree = MdForest( List( Branch(List( Leaf(Oli(List(rawHtml("order 1")),0),0), Leaf(Oli(List(rawHtml("order 2")),0),0), Branch(List( Leaf(Uli(List(rawHtml("nest2Unorderd 4")),2),2) ), 2), Leaf(Oli(List(rawHtml("order 3")),0),0)  ) ,0) ))

  val ol_empty_in = ">          \n"
}

class listParserSpec extends ListParserTests { def is = s2"""

 This is a specification to check the 'Markdown List' parser

 The 'listParser'  should
   parse a ordered or unordered list into an MdList  $e1
   join up multiple consecutive lines of  li's into one Forest or MDLists $e2
   handle mixed stuff ordered or unordered $e3
   handle nested lists of any kind ordered or unordered $e4
   handle deeply nested lists of any kind ordered or unordered $e5
   handle wierdly nested lists $e6
   handle nesting after a few rounds of not nesting $e7
                                                                 """
}


trait ListParserTests extends Specification with MustMatchers with LiFixtures {

  import markdownParser._

  def e1 = {
    val foo = listParser.run(l1_in)
    val r1 = listParser.run(l1_in) match {
      case ParseOk(i, v) => v must_== (l1_tree)
      case ParseKo(oops) => oops must_== ("fail")
//      case default => "oops" must_== ("fail")
    }
    val r2 = listParser.run(l2_in) match {
      case ParseOk(i, v) => v must_== (l2_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1 && r2
  }

  def e2 = {
    val r1 = listParser.run(l3_in) match {
      case ParseOk(i, v) => v must_== (l3_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    val r2 = listParser.run(l4_in) match {
      case ParseOk(i, v) => v must_== (l4_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1 && r2
  }

  def e3 = {
    val r1 = listParser.run(l5_in) match {
      case ParseOk(i, v) => v must_== (l5_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1
  }

  def e4 = {
    val r1 = listParser.run(l7_in) match {
      case ParseOk(i, v) => v must_== (l7_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    val r2 = listParser.run(l8_in) match {
      case ParseOk(i, v) => v must_== (l8_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1 & r2
  }

  def e5 = {
    val r1 = listParser.run(l9_in) match {
      case ParseOk(i, v) => v must_== (l9_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    val r2 = listParser.run(l10_in) match {
      case ParseOk(i, v) => v must_== (l10_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1 && r2
  }

  def e6 = {
    val r1 = listParser.run(l11_in) match {
      case ParseOk(i, v) => v must_== (l11_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1
  }


  def e7 = {
    val r1 = listParser.run(l12_in) match {
      case ParseOk(i, v) => v must_== (l12_tree)
      case ParseKo(oops) => oops must_== ("fail")
      case default => "oops" must_== ("fail")
    }
    r1
  }


}
