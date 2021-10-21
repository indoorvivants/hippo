package hippo.backend

import scala.concurrent.duration.*

import cats.effect.*

import org.http4s.HttpRoutes
import org.http4s.StaticFile
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*

import org.http4s.circe.CirceEntityDecoder
import cats.*
import org.http4s.dsl.Http4sDsl
import hippo.shared.profile.*
import io.circe.syntax.*
import cats.data.Kleisli
import org.http4s.{Response, Request}
import cats.syntax.all.*
import org.http4s.dsl.*

class Routes(
    service: HeapExplorerService,
    frontendJS: String
) extends Http4sDsl[IO]:
  val printErrors: Throwable => Kleisli[IO, Request[IO], Response[IO]] = ex =>
    Kleisli.liftF {
      IO(println(ex)) *> IO.raiseError(ex)
    }
  def routes = HttpRoutes
    .of[IO] {
      case request @ GET -> Root / "api" / "stringId" / sid =>
        service
          .getString(StringId.fromLong(sid.toLong))
          .flatMap(res => Ok(res.asJson))

      case request @ GET -> Root / "api" / "summary" =>
        service.getSummary
          .flatMap(res => Ok(res.asJson))

      case request @ GET -> Root / "frontend" / "app.js" =>
        StaticFile
          .fromResource[IO](frontendJS, Some(request))
          .getOrElseF(NotFound())

      case request @ GET -> Root / "api" / "search" / "stringByPrefix" / search =>
        service
          .stringByPrefix(search)
          .flatMap(res => Ok(res.asJson))
      
      case request @ GET -> Root / "assets" / path if staticFileAllowed(path) =>
        StaticFile
          .fromResource("/assets/" + path, Some(request))
          .getOrElseF(NotFound())

      case request @ GET -> Root =>
        StaticFile
          .fromResource[IO]("index.html", Some(request))
          .getOrElseF(NotFound())

      case request @ GET -> anything =>
        StaticFile
          .fromResource[IO]("index.html", Some(request))
          .getOrElseF(NotFound())

    }
    .orNotFound
    .handleErrorWith(printErrors(_))

  private def staticFileAllowed(path: String) =
    List(".gif", ".js", ".css", ".map", ".html", ".webm").exists(path.endsWith)
end Routes
