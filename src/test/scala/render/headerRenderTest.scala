package render

import org.specs2._
import org.specs2.matcher.MustMatchers

class HeaderRenderSpec extends HeaderRenderTests { def is = s2"""

 This is a specification to check the 'Markdown Header' parser

 The 'headerParser' string should
   parse a line starting with a '#' as a h1                         $e1
   parse a line starting with a '##' as a h2                         $e2
   parse a line starting with a '###' as a h3                         $e3
   parse a line starting with a '####' as a h4                         $e4
   parse a line starting with a '#####' as a h5                         $e5
   parse a line starting with a '######' as a h6                         $e6
   parse a line starting with a '#######' as a h7                         $e7
   parse a line starting with a '#' and containing **thing** as a h1 with nested Bold $e8


                                                                 """
/*
strip any whitespace after the '#'s before the content           $e2
   strip the '#'s from the content prefix                           $e3
   replave acnewline inline with a Br if it exits                   $e4
   even if it is the only thing inline                              $e5
   Fail if ther is no content in the header                         $e6

 */
}

trait HeaderRenderTests extends Specification with MustMatchers with HeaderRenderFixtures {

  import transformers.MarkdownToHtml._
  import transformers.Transformer._


  def e1 =  simple ---* h1_in must_== h1_val
  def e2 =  simple ---* h2_in must_== h2_val
  def e3 =  simple ---* h3_in must_== h3_val
  def e4 =  simple ---* h4_in must_== h4_val
  def e5 =  simple ---* h5_in must_== h5_val
  def e6 =  simple ---* h6_in must_== h6_val
  def e7 =  simple ---* h7_in must_== h7_val

  def e8 =  simple ---* h1_in_nest must_== h1_val_nest



}


trait HeaderRenderFixtures {
  val h1_in = "# This is a simple h1 line"
  val h1_val = "<h1>This is a simple h1 line</h1>\n"

  val h2_in = "## This is a simple h2 line"
  val h2_val = "<h2>This is a simple h2 line</h2>\n"

  val h3_in = "### This is a simple h3 line"
  val h3_val = "<h3>This is a simple h3 line</h3>\n"

  val h4_in = "#### This is a simple h4 line"
  val h4_val = "<h4>This is a simple h4 line</h4>\n"

  val h5_in = "##### This is a simple h5 line"
  val h5_val = "<h5>This is a simple h5 line</h5>\n"

  val h6_in = "###### This is a simple h6 line"
  val h6_val = "<h6>This is a simple h6 line</h6>\n"

  val h7_in = "####### This is a simple h7 line"
  val h7_val = "<h7>This is a simple h7 line</h7>\n"


  val h1_in_nest = "# This is a simple **h1** line"
  val h1_val_nest = "<h1>This is a simple <strong>h1</strong> line</h1>\n"

  val h1_nl_in = "# This is a simple **h1** line with a newline char\n"

  val h1_ns_in = "#This is a simple **h1** line with no spaces after the # prefix"

  val h1_newline_only_in = "#          \n"

  val h1_empty_in = "#                "

}
