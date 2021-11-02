package hippo.frontend
import com.raquo.laminar.api.L
import io.circe.Codec

import hippo.shared.profile.*
import com.raquo.waypoint.*
import io.circe.syntax.*

enum Page derives Codec.AsObject:
  case ClassPage(classId: ClassId)
  case StringPage(stringId: StringId)
  case ArrayDumpPage(arrayId: ArrayId)
  case MainPage

  def title =
    this match
      case _: ClassPage     => "Hippo: class page"
      case _: StringPage    => "Hippo: string pages"
      case _: ArrayDumpPage => "Hippo: array dump"
      case _                => "Hippo"

object Page:
  def toStr(p: Page) = p.asJson.noSpacesSortKeys

  def fromStr(s: String) =
    import io.circe.parser.parse
    import io.circe.syntax.given
    parse(s)
      .getOrElse(throw new RuntimeException(s"Unrecognised page: $s"))
      .as[Page]
      .getOrElse(throw new RuntimeException(s"Unrecognised page: $s"))

  val router = new Router[Page](
    routes = List(
      Route[ClassPage, Long](
        encode = userPage => userPage.classId.id.value,
        decode = arg => ClassPage(ClassId.fromLong(arg)),
        pattern = root / "page" / "class" / segment[Long] / endOfSegments
      ),
      Route[StringPage, Long](
        encode = userPage => userPage.stringId.id.value,
        decode = arg => StringPage(StringId.fromLong(arg)),
        pattern = root / "page" / "string" / segment[Long] / endOfSegments
      ),
      Route[ArrayDumpPage, Long](
        encode = arrayPage => arrayPage.arrayId.id.value,
        decode = arg => ArrayDumpPage(ArrayId.fromLong(arg)),
        pattern = root / "page" / "array" / segment[Long] / endOfSegments
      ),
      Route.static(MainPage, root / endOfSegments)
    ),
    getPageTitle = _.title,
    serializePage = Page.toStr(_),
    deserializePage = Page.fromStr(_)
  )(
    $popStateEvent = L.windowEvents.onPopState,
    owner = L.unsafeWindowOwner
  )
end Page
