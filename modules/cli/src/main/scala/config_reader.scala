package hippo
package cli

import hippo.backend.ServerConfig

import com.monovore.decline.*
import cats.instances.all.*
import cats.data.*
import com.comcast.ip4s.*
import cats.syntax.all.*

object ConfigReader:
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
end ConfigReader
