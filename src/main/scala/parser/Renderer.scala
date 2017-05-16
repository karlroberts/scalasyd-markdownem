package parser

/**
  * Typeclass for something that can render
  * Created by robertk on 14/05/17.
  */
trait CanRender[T] {
  def render(r: T): String
}



object CanRender {
  // summoner
  def apply[T](implicit evidence: CanRender[T]): CanRender[T] = evidence

//  // syntax pimps
//  implicit class CanRenderOps[T: CanRender](foo: T) {
//
//    // magic render wand
//    def ---*[T: CanRender] = {
//      val r = implicitly[CanRender[T]]
//      r.render(foo)
//    }
//  }
}

case class Renderer[A](render:  A => String) {

}

object Renderer {
  implicit val rendererToCanRender: CanRender[Renderer[_]] = new CanRender[Renderer[_]] {
    def render(r: Renderer[_]) = ???
  }
}