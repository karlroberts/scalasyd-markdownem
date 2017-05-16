package transformers

import parser._

/**
  * Created by robertk on 14/05/17.
  */
abstract  class transformer[T](run: String => T)

case class parserTransformer[A,B](parser: Parser[A])(render: ParseState[A] => B)
  extends transformer(input => {render(parser.run(input))}) {

}

object markdownstuff {
//  import parser.markdownParser.markdownPageParser
//  val markdownTransformer = parserTransformer[Markdown, String](parser)(parseState => {
//
//    def renderHtml(md: Markdown): String = {
//      md match {
//
//      }
//    }
//
//    parseState match {
//      case ParseKo(message) => message
//      case ParseOk(in, markdown) => renderHtml(markdown)
//    }
//  })
}
