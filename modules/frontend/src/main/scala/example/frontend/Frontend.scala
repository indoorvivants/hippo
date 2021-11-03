package hippo.frontend

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object Frontend:
  def main(args: Array[String]): Unit =
    documentEvents.onDomContentLoaded.foreach { _ =>
      render(dom.document.getElementById("appContainer"), Render.app)
    }(unsafeWindowOwner)
end Frontend
