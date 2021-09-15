package hippo.frontend
import com.raquo.laminar.api.L.*
import com.raquo.waypoint.Router
import example.frontend.components.SearchBox

def renderPage(page: Page)(using Router[Page]) =
  page match
    case Page.ClassPage(classId) =>
      div("Class page " + classId.toString)
    case Page.StringPage(strId) =>
      div("String page")
    case Page.MainPage =>
      val sb = SearchBox.create

      div(
        h2("Search strings (by prefix only for now)"),
        sb.node,
        child <-- sb.signal.flatMap { search =>
          if search.isEmpty then Signal.fromValue(b("..."))
          else
            Signal.fromFuture(FutureApi.searchStrings(search)).map { res =>
              res match
                case Some(Right(ls)) =>
                  ul(
                    ls.map(rd => li(rd.toString))
                  )
                case other => div(other.toString)
            }
        }
      )

def magicLink(page: Page, text: String)(using router: Router[Page]) = a(
  href := router.absoluteUrlForPage(page),
  onClick.preventDefault --> (_ => router.pushState(page)),
  text
)

object Render:
  val app: Div =
    given router: Router[Page] = Page.router
    div(
      h1("Hipster"),
      child <-- router.$currentPage.map(renderPage)
    )
