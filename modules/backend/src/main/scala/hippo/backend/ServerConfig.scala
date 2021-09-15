package hippo.backend

import cats.data.Validated
import cats.implicits.*

import com.comcast.ip4s.*
import com.monovore.decline.*

case class ServerConfig(
    host: Host,
    port: Port,
    mode: String,
    hprof: String
)

object ServerConfig:
  private val DefaultHost = "0.0.0.0"
  private val DefaultPort = 9000
  private val DefaultMode = "dev"

  private val hostOpt = Opts
    .option[String]("host", help = "Host to bind to")
    .withDefault(DefaultHost)
    .mapValidated(raw =>
      Validated
        .fromOption(Host.fromString(raw), "host is invalid")
        .toValidatedNel
    )

  private val portOpt =
    Opts
      .option[Int]("port", help = "Port to bind to")
      .withDefault(DefaultPort)
      .mapValidated(raw =>
        Validated
          .fromOption(Port.fromInt(raw), "port is invalid")
          .toValidatedNel
      )

  private val modeOpt = Opts
    .option[String]("mode", help = "Mode (dev or prod)")
    .withDefault(DefaultMode)
    .validate("must be one of: dev, prod")(Set("dev", "prod").contains)

  private val fileOpt = Opts.option[String]("hprof", help = "Heap dump")

  def apply: Command[ServerConfig] =
    Command("", "Run backend and assets server")(
      (hostOpt, portOpt, modeOpt, fileOpt).mapN(ServerConfig.apply)
    )
end ServerConfig
