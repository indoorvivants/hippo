package hippo.frontend
import com.raquo.laminar.api.L.*

def renderPage(page: Page) =
  page match
    case Page.ClassPage(classId) => 
      Signal.fromValue(div("Class page " + classId.toString))
    case Page.StringPage(strId) =>
      Signal.fromFuture(FutureApi.getString(strId)).map { v =>
        div(v.toString)
      }

object Render:
  val app: Div =
    div(
      h1("Routing App"),
      child <-- Page.router.$currentPage.flatMap(renderPage)
    )
