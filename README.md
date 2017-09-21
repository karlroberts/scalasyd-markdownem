# Markdownem - Javascript markdown parser

created using scala.js and monadic combinator parsers

### This is working demo code
This code was the state of play of the working parser as demonstrated
in my 13th September 2017 talk at scalasyd meetup.

FYI the talk slides are at [karlcode blog](http://karlcode.owtelse.com/blog/2017/07/09/scala-js-markdown-combinator-parser/?mode=deck#slide-0)

usage is demonstrated in the talk.

### Please note
This code is as-was when I gave the talk and will remain that way so people can view the talk and try it themselves.

for the latest code head on over to my [markdownem bitbucket repo](https://bitbucket.org/suited/markdownem)

pull requests at [my current repo](https://bitbucket.org/suited/markdownem) are welcome :-)

### Please also note
To run the javascript test.html in a browser be sure to uncomment the [import at line 3 in MarkdownToHtml.scala](https://github.com/karlroberts/scalasyd-markdownem/blob/master/src/main/scala/transformers/MarkdownToHtml.scala#L3) and also the [annotation at line 83 in MarkdownToHtml.scala](https://github.com/karlroberts/scalasyd-markdownem/blob/master/src/main/scala/transformers/MarkdownToHtml.scala#L83)

then recompile at the sbt prompt

    sbt:Markdownem> fullOptJS


Cheers

Karl
