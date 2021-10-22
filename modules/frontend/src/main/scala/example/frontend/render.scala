package hippo.frontend
import com.raquo.laminar.api.L.*
import com.raquo.waypoint.Router
import example.frontend.components.SearchBox
import hippo.shared.profile.*
import scodec.bits.ByteVector

def renderByteVector(bv: ByteVector) =
  val sb   = StringBuilder()
  val repr = bv.toHex
  var i    = 0
  while i < repr.length do
    sb.append(repr.slice(i, i + 1))
    if i > 0 && i % 2 == 0 then sb.append(" ")
    if i > 0 && i % 100 == 0 then sb.append(" \n")
    i += 1

  pre(code(sb.result))

def renderPage(page: Page)(using Router[Page]) =
  page match
    case Page.ClassPage(classId) =>
      div("Class page " + classId.toString)
    case Page.StringPage(strId) =>
      div(
        h2("String"),
        strong("ID: "),
        span(strId.id.value.toString),
        div(
          child <-- Api.getString(strId).map {
            case None            => i("loading")
            case Some(Left(err)) => b(err.toString)
            case Some(Right(StringData.Valid(s))) =>
              div(
                "✅ Valid UTF-8 String",
                pre(code(s))
              )

            case Some(Right(StringData.Invalid(bytes))) =>
              div(
                "❌ Invalid UTF-8 data stored in String space",
                renderByteVector(bytes.toByteVector)
              )
          }
        )
      )
    case Page.MainPage =>
      val sb = SearchBox.create

      div(
        div(
          child <-- Api.getSummary.map(_.get.right.get).map(renderSummary)
        ),
        h2("Search strings (by prefix only for now)"),
        sb.node,
        child <-- sb.signal.flatMap { search =>
          if search.isEmpty then Signal.fromValue(b("..."))
          else
            Api.searchStrings(search).map { res =>
              res match
                case Some(Right(lst)) =>
                  ul(
                    lst.take(100).collect {
                      case RecordData.Strings(sid, StringData.Valid(s)) =>
                        li(magicLink(Page.StringPage(sid), s))
                    }
                  )
                case other => div(other.toString)
            }
        }
      )

def renderSummary(sums: Summary) =
  val rendered  = sums.recordTypes.map(_._2).map(i => s"$i records")
  val maxLength = rendered.map(_.length).max
  val padded    = rendered.map(_.padTo(maxLength, ' '))
  ul(
    sums.recordTypes.zip(rendered).map { case ((tag, _), rnd) =>
      li(pre(b(rnd), nbsp, span(tag.toString)))
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
