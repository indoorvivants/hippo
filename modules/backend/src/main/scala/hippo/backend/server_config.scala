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
