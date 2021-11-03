package hippo.shared.profile
import io.circe.Codec

case class Summary(
    recordTypes: List[(Tag, Int)],
    heapDataTypes: List[(String, Int)],
    biggestArrays: List[PrimitiveArrayGist]
) derives Codec.AsObject

case class PrimitiveArrayGist(typ: BasicType, size: Int, id: ArrayId)
    derives Codec.AsObject
