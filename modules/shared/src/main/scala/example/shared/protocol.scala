package hippo.shared.profile
import io.circe.Codec

case class Summary(
    recordTypes: List[(Tag, Int)]
) derives Codec.AsObject
