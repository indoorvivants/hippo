package hippo.frontend

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import com.raquo.waypoint.Router

/** This is the most boring part of the codebase.
  *
  * The entry point which is launched by Scala.js to mount our application into
  * an existing <div> element on the HTML page
  */

object Frontend:
  val app: Div =
    given router: Router[Page] = Page.router
    div(
      child <-- router.$currentPage.map(renderPage)
    )

  def main(args: Array[String]): Unit =
    documentEvents.onDomContentLoaded.foreach { _ =>
      render(dom.document.getElementById("appContainer"), app)
    }(unsafeWindowOwner)
end Frontend
