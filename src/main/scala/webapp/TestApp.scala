package tutorial.webapp

import parser._
import parser.markdownParser._
import transformers._
import transformers.Transformer._
import transformers.MarkdownToHtml._

object TestApp extends App {

  @Override
  def mainfoo(): Unit = {
    val content =
      """
        |
        |# h1 Heading 8-)
        |## h2 Heading
        |### h3 Heading
        |#### h4 Heading
        |##### h5 Heading
        |###### h6 Heading
        |

        |## Emphasis
        |
        |**This is bold text**
        |
        |__This is bold text__
        |
        |*This is italic text*
        |
        |_This is italic text_
        |
        |~~Strikethrough~~
        |
        |
        |## Blockquotes
        |
        |> Blockquotes can also be nested...
        |> more block
        |
        |
        |## Lists
        |
        |Unordered
        |
        |+ Create a list by starting a line with `+`, `-`, or `*`
        |+ Sub-lists are made by indenting 2 spaces:
        |  - Marker character change forces new list start:
        |    * Ac tristique libero volutpat at
        |    + Facilisis in pretium nisl aliquet
        |    - Nulla volutpat aliquam velit
        |+ Very easy!
        |
        |Ordered
        |
        |1. Lorem ipsum dolor sit amet
        |2. Consectetur adipiscing elit
        |3. Integer molestie lorem at massa
        |
        |
        |1. You can use sequential numbers...
        |1. ...or keep all the numbers as `1.`
        |
        |Start numbering with offset:
        |
        |57. foo
        |1. bar
        |
        |
        |## Code
        |
        |Inline `code`
        |
        |Indented code
        |
        |    // Some comments
        |    line 1 of code
        |    line 2 of code
        |    line 3 of code
        |
        |
        |
        |
        |## Links
        |
        |[link text](http://dev.nodeca.com)
        |
        |[link with title](http://nodeca.github.io/pica/demo/ "title text!")
        |
        |Autoconverted link https://github.com/nodeca/pica (enable linkify to see)
        |
        |
        |## Images
        |
        |![Minion](https://octodex.github.com/images/minion.png)
        |![Stormtroopocat](https://octodex.github.com/images/stormtroopocat.jpg "The Stormtroopocat")
        |
        |Like links, Images also have a footnote style syntax
        |
        |![Alt text][id]
        |
        |With a reference later in the document defining the URL location:
        |
        |[id]: https://octodex.github.com/images/dojocat.jpg  "The Dojocat"
        |
        |
        |## Plugins
        |
        |The killer feature of `markdown-it` is very effective support of
        |[syntax plugins](https://www.npmjs.org/browse/keyword/markdown-it-plugin).
        |
        |
        |### [Emojies](https://github.com/markdown-it/markdown-it-emoji)
        |
        |
        |see [how to change output](https://github.com/markdown-it/markdown-it-emoji#change-output) with twemoji.
        |
        |
        |
        |
        |
        |### [Abbreviations](https://github.com/markdown-it/markdown-it-abbr)
        |
        |This is HTML abbreviation example.
        |""".stripMargin

    {
      val start = System.currentTimeMillis()

      val ret = simple ---* content
      val stop = System.currentTimeMillis()
      println(s">>>>>>>>>>> ms: -> ${stop - start}")
    }

    {
      val start = System.currentTimeMillis()

      val ret = simple ---* content
      val stop = System.currentTimeMillis()
      println(s">>>>>>>>>>> ms: -> ${stop - start}")
    }

    {
      val start = System.currentTimeMillis()

      val ret = simple ---* content
      val stop = System.currentTimeMillis()
      println(s">>>>>>>>>>> ms: -> ${stop - start}")
    }
    {
      val start = System.currentTimeMillis()

      val ret = simple ---* content
      val stop = System.currentTimeMillis()
      println(s">>>>>>>>>>> ms: -> ${stop - start}")
    }
    {
      val start = System.currentTimeMillis()

      val ret = simple ---* content
      val stop = System.currentTimeMillis()
      println(s">>>>>>>>>>> ms: -> ${stop - start}")
    }
    {
      val start = System.currentTimeMillis()

      val ret = simple ---* content
      val stop = System.currentTimeMillis()
      println(s">>>>>>>>>>> ms: -> ${stop - start}")
//      println(ret)
    }

  }

  mainfoo()

}
















// vim: set ts=4 sw=4 et:
