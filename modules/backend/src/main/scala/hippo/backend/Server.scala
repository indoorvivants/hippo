package hippo.backend

import cats.effect.*

import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.GZip
import fs2.io.file.Path

object Server extends IOApp:
  def resource(service: HeapExplorerService, config: ServerConfig) =
    val frontendJS = config.mode + ".js"
    val routes     = new Routes(service, frontendJS).routes

    val app = GZip(routes)

    EmberServerBuilder
      .default[IO]
      .withPort(config.port)
      .withHost(config.host)
      .withHttpApp(app)
      .build

  def run(args: List[String]): IO[ExitCode] =
    ServerConfig.apply.parse(args) match
      case Left(help) =>
        IO.delay(println(help)).as(ExitCode.Error)
      case Right(config) =>
        val status = IO.delay(
          println(
            s"Running server on http://${config.host}:${config.port} (mode: ${config.mode})"
          )
        )

        val fileRead =
          fs2.io.file
            .Files[IO]
            .readAll(Path(config.hprof))
            .compile
            .to(Array)
            .map(scodec.bits.ByteVector(_))
            .flatMap { bv =>
              IO.println(bv.take(5)) *>
                IO.println("Starting to read the file...") *>
                IO.blocking(hippo.analyse.Analyser.analyse(bv)) <*
                // IO.pure(hippo.shared.profile.HeapProfile.empty) <*
                IO.println("Finished reading the file")
            }
            .map(HeapExplorerService.Impl(_))

        Resource
          .eval(fileRead)
          .flatMap(service => resource(service, config))
          .use(_ => status *> IO.never)
          .as(ExitCode.Success)
end Server
