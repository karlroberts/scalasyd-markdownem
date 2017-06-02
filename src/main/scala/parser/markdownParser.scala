package parser

import ast.{MarkdownDoc, Uri}
import parser.Parser.{isNot, list1}
import parser.markdownParser.inlineParser

import scala.annotation.tailrec

/**
  * Parse Markdown into a parse tree
  * Created by robertk on 13/05/17.
  */
object markdownParser {
  import ast._

  import Parser._
  import Li._

  val inLineStartTokens = "\n*_~[`".toList

  // parse a String into a structure containing a list of Markdown instructions and a Small Symbol table that can fixup refLinks
  def markdownDocumentParser: Parser[MarkdownDoc] = {
    Parser(input => {
      import Parser._
      val parsed = parseAll(markdownASTParser, input)
      //mutabl ref to imutable map ... may mak emutable as internal use only?
      var symtab = Map[String, Uri]()


      //todo maybe make parseAll take a function to do stuff if Link rhather than walk the list twice, either fire a sideefect or return a state/symtab object as well.
      // warning side-effect on symtab
      parsed match {
        case Left(m) => ParseKo(m)
        case Right(mdlist) => {
          // look at the list and build up the symtable if we need extra knowlege from the parse.
          mdlist foreach
            {
              case m: RefLinkUri => symtab = symtab + (m.refKey -> m.uri)
              case _ => ;
            }

          ParseOk("", MDDoc(mdlist, symtab))
        }
      }

    })

  }


  def markdownDocumentParser2: Parser[MarkdownDoc] = {
    Parser(input => {
      import Parser._
      val parsed = markdownParser.run(input)
      //mutabl ref to imutable map ... may mak emutable as internal use only?
      var symtab = Map[String, Uri]()


      //todo maybe make parseAll take a function to do stuff if Link rhather than walk the list twice, either fire a sideefect or return a state/symtab object as well.
      // warning side-effect on symtab
      parsed match {
        case ParseKo(m) => ParseKo(m)
        case ParseOk(rest, mdlist) => {
          // look at the list and build up the symtable if we need extra knowlege from the parse.
          mdlist foreach
            {
              case m: RefLinkUri => symtab = symtab + (m.refKey -> m.uri)
              case _ => ;
            }

          ParseOk("", MDDoc(mdlist, symtab))
        }
      }

    })

  }


  def markdownParser: Parser[List[Markdown]] = for {
    mds <- list1(markdownASTParser)
  } yield mds

  def markdownASTParser: Parser[Markdown] =
    blockParser ||| inlineParser

  def blockParser: Parser[Markdown] =
    paragraphParser ||| brParser ||| headerParser ||| blockquoteParser ||| blockCodeParser ||| listParser ||| hardwrapParser

  def inlineParser: Parser[Markdown] =
     emphasisParser ||| imageParser ||| hyperlinkParser ||| inlineCodeParser ||| rawHtmlParser

  // test image before link as it is same except with ! as a prefix
  def imageParser: Parser[Markdown] = imgParser  ||| refImgParser ||| refLinkUriParser
  def hyperlinkParser: Parser[Markdown] = linkParser  ||| refLinkParser ||| refLinkUriParser

  /**
    * Parse Markdown headers. eg
    *
    * # this is a h1 header
    * ### this is a h3 header
    * becomes
    * <h1>this is a header</h1>
    * <h3>this is a header</h3>
    *
    * h6 is the smalles it will go, ie 10 '#'s will be <h6/>
    *
    */
  def headerParser: Parser[Markdown] = for {
    _ <- list(space)
    hashes <- list1(is('#'))
    _ <- list(space)
    md <- list1(inlineParser)
    _ <- (is('\n') ||| value('\n')) // consume a new line or just succeed, we throw away last parse anyway
  }
    yield hashes.size match {
    case 1 => H1(md)
    case 2 => H2(md)
    case 3 => H3(md)
    case 4 => H4(md)
    case 5 => H5(md)
    case 6 => H6(md)
    case 7 => H7(md)
    case default => H7(md) // too many #'s so settle on 6
  }


  /**
    * Block quotes can cover multi lines but must be presunted in one block quote eg
    * *
    * > this is line1
    * > this is line2
    * *
    * this should produce
    * *
    * <blockquote>this is line1<br/>this is line2</blockquote>
    **/
  /**
    * parses a single line of blockquote
    */
  private def simpleblockquoteParser: Parser[SimpleBlockquote] = for {
    _ <- list(space)
    tag <- is('>')
    _ <- list(space)
    md <- list1(inlineParser)
    _ <- (is('\n') ||| value('\n')) // consume a new line or just succeed, we throw away last parse anyway
  } yield SimpleBlockquote(md)

  /**
    * runs simpleBlocquoteParser as many times as it can to parse all the consecutive lines of '> text'
    * joins up the results with '\n' as a single string and re-wraps in a single blockqote
    */
  def blockquoteParser: Parser[Markdown] = {
    Parser(input => {
      val parsed: ParseState[List[SimpleBlockquote]] = (list1(simpleblockquoteParser)).run(input)
      parsed match {
        case ParseKo(m) => ParseKo(s"Not a blockquote, expected '> text', error message: $m")
        case ParseOk(i, bqs) => {
          val bqContent = bqs flatMap (  _.value :+ Br  )
          ParseOk(i, Blockquote(bqContent))
        }
      }
    })
  }

  def simpleblockCodeParser: Parser[String] = for {
    _ <- space
    _ <- space
    _ <- space
    _ <- space
    code <- list(isNot('\n'))
    _ <- (is('\n') ||| value('\n')) // consume a new line or just succeed, we throw away last parse anyway
  } yield code.mkString

  def blockCodeParser : Parser[Markdown] = {
    Parser(input => {
      val parsed: ParseState[List[String]] = (list1(simpleblockCodeParser)).run(input)
      parsed match {
        case ParseKo(m) => ParseKo(s"Not a BlockCode, expected '4 x spaces then text', error message: $m")
        case ParseOk(i, bcs) => {
          val code = bcs mkString("\n")
          ParseOk(i, BlockCode(code))
        }
      }
    })
  }

  def inlineCodeParser: Parser[Markdown] = for {
    _ <- is('`')
    code <- list1(isNot('`'))
    _ <- is('`')
  } yield InlineCode(code.mkString)


  //parse a line to see if it is an un-ordered list item
   def uLIparser: Parser[Li] = for {
    indent <- list(space)
    _ <- isIn(List('*','+','-'))
    _ <- list1(space)
    md <- list1(inlineParser)
//    md <- inlineParser.usemap[List[Char]](list1(isNot('\n')))( _.mkString )
    _ <-  (is('\n') ||| value('\n'))
  } yield uli(md, indent.size)

  //parse a line to see if it is an ordered list item
   def oLIparser: Parser[Li] = for {
    indent <- list(space)
    _ <- list1(digit)
    _ <- is('.')
    _ <- list1(space)
    md <- list1(inlineParser)
//    md <- inlineParser.usemap[List[Char]](list1(isNot('\n')))( _.mkString )
    _ <- (is('\n') ||| value('\n')) // consume a new line or just succeed, we throw away last parse anyway
  } yield oli(md, indent.size)


  // parse multilines if each line is some kind of li's
  def liParser: Parser[List[Li]] = for {
  //    lis <- list1(uLIparser) ||| list1(oLIparser)
    lis <- list1(uLIparser ||| oLIparser)
  } yield lis


  //create a rose tree of ul or li because we can have nested lists of either type
  def listParser: Parser[Markdown] = {
    import Tree._


    // a function to convert a List[li] into a Parser used in a flatmap
    def rose(lis: List[Li]): Parser[Markdown] = {

      @tailrec
      def rec(tail: List[Li], parentStack: List[Tree[Li]], listForest: List[Tree[Li]]): List[Tree[Li]] = {
        import Li._

        (tail, parentStack) match {

          // base case  return the listForest
          case (Nil, Nil) => listForest

          //pop the parent stack and recurse
          case (Nil, p :: ps) => {
            //we know parentstack is not empty so pop the head off mak it the forest as stuffu current forest in as a branch then recurse
            // the list forest  should also not be empty or else why is there a parent, but test to be sure
            val poppedParent = if(!listForest.isEmpty) {p ++ listForest.head} else p
            rec(tail, ps, poppedParent::listForest.tail)
          }

          // attache the lii to the correct place and recurse down lis list
          case (lii :: rest, _) => {
            (lii, listForest) match {

              //First tree => prepend to the forest but convert it to a Branch if it is a leaf so the renderer will surround the leaf li with the correct list type
              case (fresh@Oli(vf, indentf), Nil) => rec(rest, parentStack, branch(List(oleaf(fresh)),indentf) :: listForest)
              case (fresh@Uli(vf, indentf), Nil) => rec(rest, parentStack, branch(List(uleaf(fresh)),indentf) :: listForest)

              //same kind same nesting => add the value as a Leaf to the current forest Head
              case (fresh@Oli(vf, indentf), t :: restForest) if indentf == t.depth && t.firstValue.getOrElse(Uli(Nil, indentf)).isOrdered  => {
                // make new lead tree with new leaf attached and replace the leading MdList
                val nodeTree =  t ++ oleaf(fresh)
                rec(rest, parentStack, nodeTree :: restForest)
              }
              case (fresh@Uli(vf, indentf), t :: restForest) if indentf == t.depth && !t.firstValue.getOrElse(Oli(Nil, indentf)).isOrdered   => {
                // make new lead tree with new leaf attached and replace the leading MdList
                val nodeTree =  t ++ uleaf(fresh)
                rec(rest, parentStack, nodeTree :: restForest)
              }

              // fresh li is more deeply nested than the last so recurse  into the Branch structure change the parent!
              //NB dont need to know specific type of t as we just save it and create a new tree od same type as fresh
              case (fresh, t :: restForest) if fresh.indent > t.depth => {
                // create a new  tree starting with this li and append it to the branch list then recurse down the branch, set the parent so we can go back
                // create a new forest whose head is this new branch
                val newHeadTree = fresh match {
                  case f@Oli(vf, indentF) => branch(List(oleaf(f)),indentF)
                  case f@Uli(vf, indentF) => branch(List(uleaf(f)),indentF)
                }
                val newForestNested = newHeadTree :: restForest
                rec(rest, t :: parentStack, newForestNested);
              }

              // end of a nested just grab the first tree in forest and addatch it to the parent tree as a branch
              // then pop back out dont consume the lis and try again
              case (fresh, t :: restForest) if fresh.indent < t.depth => {
                //pass same back in but pop out to the parent
                parentStack.headOption match {
                  case None => {
                    // if we were indented but have no parent then user started indented. we need to create the implied outer list
                    // of the same type as fresh, but add the nested tree as first child then recurs without consuming the list as weare recursing back out a level
                    // let next recursion deal with fresh as normal.
                    // nb no need to take tail of parent stack as it was an empty list head is None :-)
                    val newForestTree: Tree[Li] = fresh match {
                      case ff@Oli(vf, indentF) => branch(List(t),indentF)
                      case ff@Uli(vf, indentF) => branch(List(t),indentF)
                    }
                    rec(tail, parentStack, newForestTree :: restForest )
                  }

                  //pop the head off and attach t.head to it, ie add the exhausted branch as node
                  // then dont consume the list and recurse back out a level
                  case Some(tree) => rec(tail, parentStack.tail, tree ++ t :: restForest)
                }
              }

              // we have a li at same level as tree but different type
              // this MUST be a new List so create a new tree in the forest and and go again.
              case (fresh, forest) => {
                val newForestTree = fresh match {
                  case t@Oli(vf, indentF) => branch(List(oleaf(t)),indentF)
                  case t@Uli(vf, indentF) => branch(List(uleaf(t)),indentF)
                }
                rec(rest, parentStack, newForestTree :: forest)
              }

            }
          }
        }
      }

      // kick off the list walking recursion, need to know if i start with an ul or ol
      //PRECONDITION before recurse we know list is not empty because liParser will have succeeded,
      // would prefer a NonEmpty List type here rather than list
      lis match {
        case Nil => failed("no List[li] to parse") //never getting here
        case li :: rest => {
          val listForest = rec(lis, Nil, Nil)
          value(MdForest(listForest.reverse))
        }

      }

    }

    //flatmap a liParser with rose to get a Parser[MdForest]
    liParser flatMap rose // map (MdListTree(_))
  }

  def paragraphParser : Parser[Markdown] = for {
    _ <- is('\n')
    _ <- list1(is('\n'))
  } yield Paragraph



  /* -------------------------------------- */
  /* ------- inline markdown ----------- */
  /* ------ all have an end marker, then consume whitespace ---------- */
  /* -------------------------------------- */
  def linkParser: Parser[Markdown] = for {
//    _ <- list(space)
    _ <- is('[')
    linkText <- inlineParser.usemap[List[Char]](list1(isNot(']')))( _.mkString )
    _ <- is(']')
    _ <- is('(')
    url <- list1(satisfy(c => !c.isWhitespace && c != ')' )) flatMap parseHelper.buildURL
    _ <- opt(space)
    title <- opt( list1(isNot(')')) map (_.mkString(""))  )
    _ <- is(')')
//    _ <- list(space) ||| value(Nil)
  } yield Link(linkText, url, title)

  // list1(satisfy(c => !c.isWhitespace && c != ')' ))
  private def refLinkRefPartParser: Parser[String] = for {
    _ <- is('[')
    reftext <- list1(isNot(']'))
    _ <- is(']')
  } yield reftext mkString

  /**
    * parse the uri from a refererred to link
    * parse [2] http://foo.ar.com
    * @return
    */
  def refLinkUriParser: Parser[Markdown] = for {
    _ <- is('[')
    linkKey <- list1(isNot(']'))
    _ <- is(']')
    _ <- list(space)
    uri <- list(visible) flatMap parseHelper.buildURL
  } yield RefLinkUri(linkKey.mkString, uri)

  def refLinkParser: Parser[Markdown] = for {
    _ <- is('[')
    linkText <- inlineParser.usemap[List[Char]](list1(isNot(']')))( _.mkString )
    _ <- is(']')
    refText <- opt(refLinkRefPartParser)
  } yield RefLink(linkText,refText.getOrElse(linkText.toString)) // TODO instead of toString, use typeclass show, we can let rawHtml show be its value


  def imgParser: Parser[Markdown] = for {
    _ <- is('!')
    _ <- is('[')
    altText <- (list1(isNot(']'))) map (_.mkString)
    _ <- is(']')
    _ <- is('(')
    uri <- list1(satisfy(c => !c.isWhitespace && c != ')' )) flatMap parseHelper.buildURL
    _ <- opt(space)
    title <- opt( list1(isNot(')')) map (_.mkString(""))  )
    _ <- is(')')
  } yield Img(altText, uri, title)

  def refImgParser: Parser[Markdown] = for {
    _ <- is('!')
    _ <- is('[')
    altText <- (list1(isNot(']'))) map (_.mkString)
    _ <- is(']')
    refText <- opt(refLinkRefPartParser)
  } yield RefImg(altText,refText.getOrElse(altText))

  /** raw HTML is just an inline String that can be printed to be consideredHTML. Just read unless we encounter an inline elemet or end of line*/
  def rawHtmlParser: Parser[Markdown] = for {
    theHtml <- list1(isNotIn(inLineStartTokens))
  } yield rawHtml(theHtml.mkString)

  def brParser: Parser[Markdown] = for {
    _ <- space
    _ <- spaces1
    _ <- (is('\n'))
  } yield Br

  def hardwrapParser: Parser[Markdown] = for {
    _ <- (is('\n'))
  } yield Hardwrap


  def emphasisParser: Parser[Markdown] = {
    // notice that italic Parser is after boldParser, this is to so **x is understood as bold not as *xx ie italic
    strikethroughParser ||| boldParser ||| italicParser
  }

  def boldParser1: Parser[Markdown] = for {
  _ <- is('*')
  _ <- is('*')
  boldtext <- list1(inlineParser)
  _ <- is('*')
  _ <- is('*')
  } yield Bold(boldtext)

  def boldParser2: Parser[Markdown] = for {
  _ <- is('_')
  _ <- is('_')
  boldtext <- list1(inlineParser)
  _ <- is('_')
  _ <- is('_')
  } yield Bold(boldtext)

  def boldParser: Parser[Markdown] = boldParser1 ||| boldParser2

  def italicParser1: Parser[Markdown] = for {
  _ <- is('*')
  boldtext <- list1(inlineParser)
  _ <- is('*')
  } yield Italic(boldtext)

  def italicParser2: Parser[Markdown] = for {
  _ <- is('_')
  boldtext <- list1(inlineParser)
  _ <- is('_')
  } yield Italic(boldtext)

  def italicParser: Parser[Markdown] = italicParser1 ||| italicParser2

  def strikethroughParser: Parser[Markdown] = for {
  _ <- is('~')
  _ <- is('~')
  boldtext <- list1(inlineParser)
  _ <- is('~')
  _ <- is('~')
  } yield Strikethrough(boldtext)


  /*

  * thing
  * thing
    1. thing
    2. foo
      - bar
  * thing


   */


  //  val linkParser: Parser[Markdown] = for {
  //    _ <-
  //  }


}

object parseHelper  {
  import Parser._
  import java.net.{URI => JURI}

  /**
    * Builds a Parser[Uri], instantiate the Uri case class, but tests the uri string using the Java URI constructor.
    * If we dont have a java exception we are cool.
    *
    * @param attemptURL
    * @return
    */
  def buildURL(attemptURL: List[Char]): Parser[Uri] =
    try{
      if(attemptURL.isEmpty) failed("The URL can not be an empty string")
      else {
        val theUrlText = attemptURL.mkString
        val url = new JURI(theUrlText)
        value(Uri(theUrlText))
      }
    }
    catch {
      case e: Throwable => failed(e.getMessage)
    }

}
