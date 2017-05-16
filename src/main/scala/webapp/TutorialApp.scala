package tutorial.webapp

import parser.{ParseKo, ParseOk, ParseState, PersonParser}

import scala.scalajs.js.JSApp

object TutorialApp extends JSApp {
//object TutorialApp extends App {

  def main(): Unit = {
    println("Hello world!")

    import parser.Parser._
    import parser.PersonParser._
    PersonParser.Data foreach {
      list(personParser).run(_) match {
        case ParseOk(i, v) => {
          println(s" OK parse, ${v.mkString(",\n")} ")
          println(s"remaining input = $i")
        }
        case ParseKo(m) => println(s"oops : $m")
      }
    }


    import parser.markdownParser._

    val s1 =
      """# this is **h1**
        |## this is h2
      """.stripMargin

    val s2 = "##this is a **h2** title\n";
    val s3 = "### this is a **h3** title";
    val s4 = "#### this is a **h4** title";
    val s5 = "##### this is a **h5** title";
    val s6 = "###### this is a **h6** title";

    val bq1 = "> this is a blockquote line\n"

    keyword("foobar").run("This is not foobar") match {
      case ParseOk(i, v) => {
        println(s" OK parse, ${v} ")
        println(s"remaining input = $i")
      }
      case ParseKo(m) => println(s"oops : $m")
    }

    keyword("foobar").run("foobar now what?") match {
      case ParseOk(i, v) => {
        println(s" OK parse, ${v} ")
        println(s"remaining input = $i")
      }
      case ParseKo(m) => println(s"oops : $m")
    }

    println(s"attempting to parse $s1")
    markdownParser.run(s1) match {
      case ParseOk(i, v) => {
        println(s"markdown OK parse, ${v.toString} ")
        println(s"markdon remaining input = $i")
      }
      case ParseKo(m) => println(s"markdown oops : $m")
    }

    println(s"attempting to parse $bq1")
    markdownParser.run(bq1) match {
      case ParseOk(i, v) => {
        println(s"markdown OK parse, ${v.toString} ")
        println(s"markdon remaining input = $i")
      }
      case ParseKo(m) => println(s"markdown oops : $m")
    }



  }

}
















// vim: set ts=4 sw=4 et:
