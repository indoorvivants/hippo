package hippo.frontend
import com.raquo.laminar.api.L
import io.circe.Codec

import hippo.shared.profile.*
import com.raquo.waypoint.*
import io.circe.syntax.*

enum Page:
  case ClassPage(classId: ClassId)
  case StringPage(stringId: StringId)
  case MainPage

object Page:
  def toStr(p: Page) =
    p match
      case ClassPage(id)  => "ClassPage:" + id.id.value.toString
      case StringPage(id) => "StringPage" + id.id.value.toString
      case MainPage       => "MainPage"

  // TODO: this is pretty bad and so much maintenance
  def fromStr(s: String) =
    val segments = s.split(":")
    segments.headOption match
      case Some("ClassPage") => ClassPage(ClassId.fromLong(segments(1).toLong))
      case Some("StringPage") =>
        StringPage(StringId.fromLong(segments(1).toLong))
      case Some("MainPage") => MainPage
      case other => throw new RuntimeException(s"Unrecognised page: $$other")

  def stringRoute = Route[StringPage, Long](
    encode = userPage => userPage.stringId.id.value,
    decode = arg => StringPage(StringId.fromLong(arg)),
    pattern = root / "page" / "string" / segment[Long] / endOfSegments
  )

  def classRoute = Route[ClassPage, Long](
    encode = userPage => userPage.classId.id.value,
    decode = arg => ClassPage(ClassId.fromLong(arg)),
    pattern = root / "page" / "class" / segment[Long] / endOfSegments
  )

  def mainRoute = Route.static(MainPage, root / endOfSegments)

  val router = new Router[Page](
    routes = List(classRoute, stringRoute, mainRoute),
    getPageTitle =
      _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage =
      Page.toStr(_), // serialize page data for storage in History API log
    deserializePage = Page.fromStr(_)
  )(
    $popStateEvent =
      L.windowEvents.onPopState, // this is how Waypoint avoids an explicit dependency on Laminar
    owner = L.unsafeWindowOwner // this router will live as long as the window
  )
end Page
