package transformers

//import scala.scalajs.js.annotation.JSExportTopLevel

import ast.Li.{Oli, Uli}
import ast.{Markdown, MarkdownDoc}
import ast.Tree.{Branch, Leaf}
import parser._

import scala.annotation.tailrec

/**
  * Created by robertk on 14/05/17.
  */
/**
  * Type class to transform a Document.
  *
  */
trait Transformer[T] {

  type IN
  type OUT

  def run(t: T, in: IN): OUT
}

object Transformer {

  // aux pattern
  type Aux[T0,IN0,OUT0] = Transformer[T0] { type IN = IN0; type OUT = OUT0 }

  // idiomatic typeclass "Summoner" so I don't need to use implicitly which would loose type info of In Out
  def apply[T,A,B](implicit evidence: Transformer[T]): Aux[T,evidence.IN,evidence.OUT] = evidence

  // idiomatic typclass "Constructor" method
  def instance[T,A,B](func: (T,A) => B): Aux[T,A,B] = {
    new Transformer[T] {
      type IN = A; type OUT = B;
      def run(t: T, in: A): B = func(t,in)
    }
  }


  // syntax pimps
  implicit class TransformerOps[T0](foo: T0) {

    /**
      * magic wand. pimp alias of Transformer.run eg a transform function
      * @param bar
      * @return
      */
    def ---*[A,B](bar: A)     (implicit aux: Transformer.Aux[T0,A,B]) : B = {
      aux.run(foo, bar)
    }

    def transform[A,B](bar: A)(implicit aux: Transformer.Aux[T0,A,B]) : B = {
      aux.run(foo, bar)
    }
  }
}

object TransformerApi {
  import Transformer._
  def transform[T,A,B](t: T)(in: A)(implicit aux: Transformer.Aux[T,A,B]) :B = {
    t ---* in
  }
}


// Specific Markdown Transformer
case class MarkdownToHtml(p: String => ParseState[MarkdownDoc], r: ParseState[MarkdownDoc] => Html) {

//  def biMap[PRE](mIn: PRE => String, mOut: )
}

object MarkdownToHtml {
  import Transformer._
  import ast._

  // Use instance to add MarkdownToHtml to the Transformer typeclass
  implicit val m2hTransformer: Transformer.Aux[MarkdownToHtml,String, parser.Html] = instance( (t, in) => t.r(t.p(in)) )

//  @JSExportTopLevel("mdmagic")
  def transform(md: String): String = simple ---* md

  import parser.markdownParser.markdownDocumentParser
  val simple = MarkdownToHtml( markdownDocumentParser.run , _ match {
    case ParseKo(error) =>
      s"""
         |<p>
        | <h1>Oops Exception parsing markdown</h1>
        |  <h5>$error</h5>
        |</p>
        |""".stripMargin

    case ParseOk(remainingInput, MDDoc( mds, refLinkMap)) => {

      val sep = sys.props("line.separator")
      val indent: Int = 2

      //Close over
      // variable to keep track of paragraph parens
      var pp: Boolean = false

      def renderRecursivly(mds: List[Markdown], acc: String, closings: List[String]): String = mds match {

        case Nil =>
          if(closings.isEmpty) acc // terminal case
          else renderRecursivly(mds, acc + closings.head, closings.tail) //run the closings down

        case m::ms => m match {
          case RawHtml(v) => renderRecursivly(ms, acc + v, closings)
          case Br => renderRecursivly(ms, acc + s"<br/>$sep", closings)
          case Paragraph => pp match {
            case false => {pp = true; renderRecursivly(ms, acc + s"$sep<p>$sep", closings)} // no previous so open paragraph
            case true => {pp = false; renderRecursivly(ms, acc + s"$sep</p>$sep", closings)} // end of a paragraph ??? TODO pos open another?
          }

          case H1(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h1>", s"</h1>$sep"::Nil), closings)
          case H2(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h2>", s"</h2>$sep"::Nil), closings)
          case H3(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h3>", s"</h3>$sep"::Nil), closings)
          case H4(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h4>", s"</h4>$sep"::Nil), closings)
          case H5(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h5>", s"</h5>$sep"::Nil), closings)
          case H6(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h6>", s"</h6>$sep"::Nil), closings)
          case H7(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<h7>", s"</h7>$sep"::Nil), closings)

          case Blockquote(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<blockquote>", s"</blockquote>$sep"::Nil), closings)
          case BlockCode(str) => renderRecursivly(ms, acc+s"<pre>$sep<code>$str</code>$sep</pre>", closings)

          case MdForest(forest) => renderRecursivly(ms, acc+renderForest(forest),  closings)



          // a newline not prefixed with two or more spaces is just hardwrap by the author just visually add a space and a newline
          case Hardwrap => renderRecursivly(ms, acc + s" $sep", closings)

          case Bold(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<strong>", s"</strong>"::Nil), closings)
          case Italic(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<em>", s"</em>"::Nil), closings)
          case Strikethrough(md) => renderRecursivly(ms, renderRecursivly(md, acc+"<s>", s"</s>"::Nil), closings)

          case Link(text, uri, title) => title match {
            case None => renderRecursivly(ms, renderRecursivly(List(text), acc+s"""<a href="${uri.uri}">""", "</a>"::Nil), closings)
            case Some(tit) => renderRecursivly(ms, renderRecursivly(List(text), acc+s"""<a href="${uri.uri}" title="$tit">""", "</a>"::Nil), closings)
          }
          case RefLink(text,key) => renderRecursivly(ms, renderRecursivly(List(text), acc+s"""<a href="${refLinkMap.getOrElse(key,Uri("http://badlink.badlink.com")).uri}">""", "</a>"::Nil), closings)

          case Img(text, uri, title) => title match {
            case None => renderRecursivly(ms, acc+s"""<img src="${uri.uri}" alt="$text">""", closings)
            case Some(tit) => renderRecursivly(ms, acc+s"""<img src="${uri.uri}" title="$tit">""", closings)
          }
          case RefImg(text,key) => renderRecursivly(ms, acc+s"""<img src="${refLinkMap.getOrElse(key,Uri("http://badlink.badlink.com")).uri}">""", closings)

          case RefLinkUri(key,Uri(uri)) => renderRecursivly(ms, acc+s"""<!-- [$key] $uri -->""", closings)

          case InlineCode(str) => renderRecursivly(ms, acc+s"<code>$str</code>", closings)
          case _ => s"WTF not implemented for $m"

        }
      }

      //helper funcs for list render
      // helper do the li with indent
      import ast.Li._

      def doIndent(depth: Int, str: String): String = s"""${" " * (indent * depth)}$str"""

      // list items can contain nested markdown so render recursivly
      def doLi(li: Li): String = renderRecursivly(li.value, doIndent(li.indent + indent, "<li>"),  s"</li>$sep"::Nil)


      //  @tailrec
      def renderTrees(trees: List[Tree[Li]], acc: String, closings: List[String]): String = trees match {
        case Nil =>
          if(closings.isEmpty) acc // terminal case
          else renderTrees(trees, acc + closings.head, closings.tail) // run down the closings

        case t::ts => t match {
          //leaf should already be in a branch's list so no need to do wrap in open or close
          case Leaf(value,depth) => renderTrees(ts, acc + doLi(value), closings)

          case Branch(branches, depth) => branches match {
            case Nil =>
              if(closings.isEmpty) acc // terminal case
              else renderTrees(branches, acc + closings.head, closings.tail) // run down the closings

            // new branch so add our opening and closing and recurse down the trees in the branch
            case b::bs => {
              val ordered = t.firstValue.getOrElse(Uli(Nil,depth)).isOrdered
              val open  = doIndent(depth, s"""<${if(ordered) "ol" else "ul"}>$sep""")
              val close = doIndent(depth, s"""</${if(ordered) "ol" else "ul"}>$sep""")
              renderTrees(ts, renderTrees(branches, acc + open, close::Nil), closings)

            }
          }
        }
      }

      def renderForest(forest: List[Tree[Li]]): String = {
//        //handle edge case where the tree is a single leaf, wrap it in a branch so the appropriate list open tags are wrapped around it
//        val newforest = forest map { t => t match {
//          case  Leaf(value,depth) => Branch(List(t),depth)
//          case _ => t
//        }}
        renderTrees(forest, "", Nil)
      }


      //Do the render
      val ret = renderRecursivly(mds,"",Nil)
      // tidy up trailing paragraph
      if(pp) ret + s"</p>$sep" else ret

    }

  } )

}

