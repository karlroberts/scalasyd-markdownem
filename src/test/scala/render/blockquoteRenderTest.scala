package render

import org.specs2._
import org.specs2.matcher.MustMatchers

class BlockquoteRenderSpec extends BQRenderRenderTests { def is = s2"""

 This is a specification to check the 'Markdown BlockQuote' parser

 The 'blockParser' string should
   render a line starting with a '>' as a blockquote                 $e1


                                                                 """
  /*
  strip any whitespace after the '>'s before the content           $e2
  strip the '>'s from the content prefix                           $e3
  strip the new line char from the end of the content if it exits  $e4
  fail if there is no visible content                              $e5
    join up multiple consecutive lines of blockquotes into one blockquote of multiple lines $e6
*/
}

trait BQRenderRenderTests extends Specification with MustMatchers with BQRenderFixtures {

  import transformers.MarkdownToHtml._
  import transformers.Transformer._


  def e1 =  simple ---* h1_in must_== h1_val

}


trait BQRenderFixtures {
  val h1_in = "> This is a simple **bq** line"
  var h1_val = "<blockquote>This is a simple <strong>bq</strong> line</blockquote>"

  val h1_nl_in = "> This is a simple **bq** line with a newline char\n"

  val h1_ns_in = ">This is a simple **bq** line with no spaces after the > prefix"

  val h1_newline_only_in = "#          \n"

  val h1_empty_in = ">                "

  val bq_multiline_in =
    """>simple bq line
      |> another bq line
      |> foofofof""".stripMargin

}
