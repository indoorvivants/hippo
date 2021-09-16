package hippo.frontend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import sttp.client3.*
import sttp.client3.circe.*
import sttp.capabilities.WebSockets
import hippo.shared.profile.*
import com.raquo.laminar.api.L.*

object Api:
  given backend: SttpBackend[Future, WebSockets] = FetchBackend()

  type Result[D] = Signal[Option[Either[Throwable, D]]]

  private def ApiHost =
    import org.scalajs.dom

    val scheme = dom.window.location.protocol
    val host   = dom.window.location.host

    s"$scheme//$host"

  def searchStrings(prefix: String): Result[List[RecordData]] =
    val req = basicRequest
      .get(uri"$ApiHost/api/search/stringByPrefix/${prefix}")
      .response(asJson[List[RecordData]])

    Signal.fromFuture(req.send(backend).map(_.body))

  def getSummary: Result[Summary] =
    val req = basicRequest
      .get(uri"$ApiHost/api/summary")
      .response(asJson[Summary])

    Signal.fromFuture(req.send(backend).map(_.body))

  def getString(
      search: StringId
  ): Result[StringData] =
    val req = basicRequest
      .get(uri"$ApiHost/api/stringId/${search.id.value.toString}")
      .response(asJson[StringData])

    Signal.fromFuture(req.send(backend).map(_.body))

end Api
