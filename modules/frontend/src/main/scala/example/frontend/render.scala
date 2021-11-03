package hippo.frontend
import com.raquo.laminar.api.L.*
import com.raquo.waypoint.Router
import hippo.frontend.components.SearchBox
import hippo.shared.profile.*
import scodec.bits.ByteVector

/** This file contains various functions that render the data and pages as
  * Laminar's reactive elements
  */

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
end renderByteVector

extension [T](st: Signal[Option[Either[Throwable, T]]])
  def handled(f: T => Element) = st.map {
    case None            => i("loading")
    case Some(Left(err)) => b(err.toString)
    case Some(Right(t))  => f(t)
  }

def renderStringPage(strId: StringId) =
  div(
    h2("String"),
    strong("ID: "),
    span(strId.id.value.toString),
    div(
      child <-- Api.getString(strId).handled {
        case StringData.Valid(s) =>
          div(
            "✅ Valid UTF-8 String",
            pre(code(s))
          )

        case StringData.Invalid(bytes) =>
          div(
            "❌ Invalid UTF-8 data stored in String space",
            renderByteVector(bytes.toByteVector)
          )
      }
    )
  )

def renderClassPage(classId: ClassId) =
  div("Class page " + classId.toString)

def renderMainPage(using Router[Page]) =
  val sb = SearchBox.create

  div(
    cls := "row",
    div(
      cls := styles.cls.records_summary,
      h2("Summary of records"),
      child <-- Api.getSummary.handled(renderSummary)
    ),
    div(
      cls := styles.cls.string_search,
      h3(cls := styles.cls.string_search_header, "Search strings"),
      p(
        cls := styles.cls.string_search_description,
        small("by prefix only (for now)")
      ),
      sb.node,
      child <-- sb.signal.flatMap { search =>
        if search.isEmpty then Signal.fromValue(b("..."))
        else
          Api.searchStrings(search).handled { lst =>
            ul(
              lst.take(100).collect {
                case RecordData.Strings(sid, StringData.Valid(s)) =>
                  li(magicLink(Page.StringPage(sid), s))
              }
            )
          }
      }
    )
  )
end renderMainPage

def renderPage(page: Page)(using Router[Page]) =
  div(
    className := styles.cls.container,
    h1(cls := styles.cls.heading, "Hippo"),
    p(
      cls := styles.cls.description,
      "Unfinished, untidy heap dump browser (it will never be finished)"
    ),
    page match
      case Page.ClassPage(classId)   => renderClassPage(classId)
      case Page.StringPage(strId)    => renderStringPage(strId)
      case Page.MainPage             => renderMainPage
      case Page.ArrayDumpPage(arrId) => renderArrayPage(arrId)
  )

def renderArrayPage(id: ArrayId) =
  div(
    child <-- Api.getPrimitiveArray(id).handled { pma =>
      div(
        pma.toString
      )
    }
  )

def renderSummary(sums: Summary)(using Router[Page]) =
  val rendered  = sums.recordTypes.map(_._2).map(i => s"$i records")
  val maxLength = rendered.map(_.length).max
  val padded    = rendered.map(_.padTo(maxLength, ' '))

  div(
    cls := "row",
    div(
      cls := styles.cls.record_types_summary,
      h4("Record types"),
      ul(
        sums.recordTypes.zip(rendered).map { case ((tag, _), rnd) =>
          val segmentRender = pre(b(rnd), nbsp, span(tag.toString))
          if tag == Tag.HeapDumpSegment then
            li(
              segmentRender,
              ul(sums.heapDataTypes.map { case (name, cnt) =>
                li(b(cnt), " ", name)
              })
            )
          else li(segmentRender)
        }
      )
    ),
    renderTopPrimitiveArrays(sums.biggestArrays)
  )
end renderSummary

def renderTopPrimitiveArrays(top: Seq[PrimitiveArrayGist])(using Router[Page]) =
  div(
    cls := styles.cls.primitive_array_summary,
    h4("Longest primitive arrays"),
    ol(
      top.map { case PrimitiveArrayGist(typ, els, id) =>
        li(
          magicLink(
            Page.ArrayDumpPage(id),
            span(b(typ.toString), s" ($els elements)")
          )
        )
      }
    )
  )

def magicLink(page: Page, text: String)(using router: Router[Page]) = a(
  href := router.absoluteUrlForPage(page),
  onClick.preventDefault --> (_ => router.pushState(page)),
  text
)

def magicLink(page: Page, text: Element)(using router: Router[Page]) = a(
  href := router.absoluteUrlForPage(page),
  onClick.preventDefault --> (_ => router.pushState(page)),
  text
)

object Render:
  val app: Div =
    given router: Router[Page] = Page.router
    div(
      child <-- router.$currentPage.map(renderPage)
    )
