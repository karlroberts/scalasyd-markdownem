package ast

import java.net.{URI => JURI}
import parser.{Html}
//import parser.Tree.leaf

sealed trait MarkdownDoc
case class MDDoc(markdown: List[Markdown], refLinkMap: Map[String,Uri]) extends MarkdownDoc
/**
  * Created by robertk on 25/07/17.
  */
/**
  * AST as an ADT
  */
sealed trait Markdown {

}

//A NOP Markdown. useful as the Zero in monoidal folds
case object empty extends Markdown


// an escape character
case class Escape(value: Char) extends Markdown

//if we have raw HTML we just output it, no need to parse it for content
case class RawHtml(value: Html) extends Markdown {
  // hack to allow refLink parser to use value of link text as ref key if one wasnt provided.
  // should use Cast Show typeclass and have an instance for each Markdown
//  override def toString: String = value
}

//just dumped a sting into here, dont know yet if it contains any markdown so inline needs to be parsed.
//case class raw(value: String) extends Markdown

// do i need this for content? or just use rawHtml?

// h1 header the value in the header could be more markdown
case class H1(value: List[Markdown]) extends Markdown

case class H2(value: List[Markdown]) extends Markdown

case class H3(value: List[Markdown]) extends Markdown

case class H4(value: List[Markdown]) extends Markdown

case class H5(value: List[Markdown]) extends Markdown

case class H6(value: List[Markdown]) extends Markdown

case class H7(value: List[Markdown]) extends Markdown

//Blockquote
case class SimpleBlockquote(value: List[Markdown])
case class Blockquote(value: List[Markdown]) extends Markdown

//a new line is a new <p></p> block i think
case object Paragraph extends Markdown

case class BlockCode(value: String) extends Markdown

//Inline elements
//Emphasid
case class Bold(value: List[Markdown]) extends Markdown

case class Italic(value: List[Markdown]) extends Markdown

case class Strikethrough(value: List[Markdown]) extends Markdown

case class InlineCode(value: String) extends Markdown


//List items returned by liParser NB these are not Markdown, but ann intermediate AST,
// listParser uses liParser to create a MDForest which is Markdown
// ADT
sealed trait Li {
  val value: List[Markdown]
  val indent: Int
  def isOrdered: Boolean
}

object Li {
  //ADT members
  case class Uli(value: List[Markdown], indent: Int) extends Li {
    val isOrdered = false
  }
  case class Oli(value: List[Markdown], indent: Int) extends Li {
    val isOrdered = true
  }

  def uli(value: List[Markdown], indent: Int): Li = Uli(value, indent)
  def oli(value: List[Markdown], indent: Int): Li = Oli(value, indent)

  //helper funcs for creating Trees of ordered or unorderd Li's with nesting added
  import Tree._
  def uleaf(uli: Uli): Tree[Li] = leaf(uli, uli.indent)
  def oleaf(oli: Oli): Tree[Li] = leaf(oli, oli.indent)
}




case class MdForest(forest: List[Tree[Li]]) extends Markdown



/**
  * Case class to represent a URI NB the param uri is expected to be a valid URI - currently not checked though urlParser does check
  * @param uri
  */
case class Uri(uri: String)
//Links
/**
  * takes [linktest](uri) produces link(linkText: Markdown, url: URL)
  *
  * @param linkText
  * @param uri
  */
case class Link(linkText: Markdown, uri: Uri, title: Option[String] = None) extends Markdown

case class Img(alt: String, url: Uri, title: Option[String] = None) extends Markdown

case object Br extends Markdown
case object Hardwrap extends Markdown

/**
  * Referece Style links show the link text but then have a further [] construct containing text that will be the key to the link later on eg
  *
  * [I'm a reference-style link][Arbitrary case-insensitive reference text]

[I'm a relative reference to a repository file](../blob/master/LICENSE)

[You can use numbers for reference-style link definitions][1]

Or leave it empty and use the [link text itself].

URLs and URLs in angle brackets will automatically get turned into links.
http://www.example.com or <http://www.example.com> and sometimes
example.com (but not on Github, for example).

Some text to show that the reference links can follow later.

[arbitrary case-insensitive reference text]: https://www.mozilla.org
[1]: http://slashdot.org
[link text itself]: http://www.reddit.com

  * @param linkText
  * @param refKey
  */
case class RefLink(linkText: Markdown, refKey: String) extends Markdown
case class RefImg(altText: String, refKey: String) extends Markdown

case class RefLinkUri(refKey: String, uri: Uri) extends Markdown


// Tree[A] ADT
sealed trait Tree[A] {

  import Tree._

  /**
    * Depth of tree, in this case use it to represent indent level of nested lists
    * @return
    */
  def depth: Int
  def firstValue: Option[A]

  def ++(that: Tree[A]) = grow(this, that)

  // tree combinator
  def grow(thisT: Tree[A], thatT: Tree[A]): Tree[A] = {
    (thisT, thatT) match {
      //same depth
      case (t1@Leaf(v, depth), t2: Tree[A])      if t2.depth == depth => Branch(List(t1, t2),                   depth)
      case (Branch(ts,depth),  t2: Tree[A])      if t2.depth == depth => Branch(ts :+    t2,                    depth)

      // 2nd deeper
      case (t1@Leaf(_, depth), t2@Leaf(_,d2))           if d2 > depth => Branch(List(t1, Branch(List(t2), d2)), depth)
      case (t1@Leaf(_, depth), t2@Branch(_,d2))         if d2 > depth => Branch(List(t1, t2),                   depth)
      case (Branch(ts,depth),  t2@Leaf(_,d2))           if d2 > depth => Branch(ts :+    Branch(List(t2), d2),  depth)
      case (Branch(ts,depth),  t2@Branch(_,d2))         if d2 > depth => Branch(ts :+    t2,                    depth)

      case (t1: Tree[A], t2: Tree[A]) if t2.depth < t1.depth  => Branch(List(Branch(List(t1),t1.depth), t2),t2.depth)
    }
  }
}

object Tree {

  //  Sum type members of the ADT Tree
  case class Leaf[A](value: A, depth: Int) extends Tree[A] {
    val firstValue = Some(value)
  }


  case class Branch[A](branch: List[Tree[A]], depth: Int) extends Tree[A]
  {
    def firstValue = branch.headOption flatMap( _.firstValue )
  }

  //helper builders that return the ADT Tree[A] rather than Leaf or Branch
  def leaf[A](value: A, depth: Int): Tree[A] = Leaf(value, depth)
  def branch[A](branches: List[Tree[A]], depth: Int): Tree[A] = Branch(branches, depth)

}


