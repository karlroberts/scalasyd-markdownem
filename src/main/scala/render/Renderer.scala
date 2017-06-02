package render

/**
  * Typeclass for something that can render
  * Created by robertk on 14/05/17.
  */
trait Renderer[T] {
  def render(r: T): String
}


object Renderer {
  // summoner
  def apply[T](implicit evidence: Renderer[T]): Renderer[T] = evidence

  // syntax pimps
  implicit class CanRenderOps[T: Renderer](foo: T) {

    // magic render wand
    def apply = {
      val r = Renderer[T]
      r.render(foo)
    }

    def ==> = apply
  }
}

//TODO add Free Monad interpreters of Parser[Markdown] here

