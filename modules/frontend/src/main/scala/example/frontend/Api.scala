package hippo.frontend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import sttp.client3.*
import sttp.client3.circe.*
import sttp.capabilities.WebSockets
import hippo.shared.profile.*

object FutureApi:
  given backend: SttpBackend[Future, WebSockets] = FetchBackend()

  private def ApiHost =
    import org.scalajs.dom

    val scheme = dom.window.location.protocol
    val host   = dom.window.location.host

    s"$scheme//$host"

  def getString(
      search: StringId,
  ): Future[Either[Throwable, StringData]] =

    val req = basicRequest
      .get(uri"$ApiHost/stringId/${search.id.value.toString}")
      .response(asJson[StringData])

    req.send(backend).map(_.body)
end FutureApi
