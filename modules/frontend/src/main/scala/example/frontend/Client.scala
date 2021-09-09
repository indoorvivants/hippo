package hippo.frontend

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object Client:
  case class SearchBox private (node: Element, signal: Signal[String])

  object SearchBox:
    def create =
      val node = input(
        `type` := "text",
        idAttr := "search-filter"
      )

      val stream =
        node.events(onInput).mapTo(node.ref.value).startWith("")

      new SearchBox(node, stream)

  def main(args: Array[String]): Unit =
    documentEvents.onDomContentLoaded.foreach { _ =>
      render(dom.document.getElementById("appContainer"), Render.app)
    }(unsafeWindowOwner)
end Client
