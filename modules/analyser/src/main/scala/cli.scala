package heappie
import java.io.*
import scodec.bits.ByteVector
import scala.util.Try

@main def analyse(filename: String) =
  val buf = new FileInputStream(
    "/Users/velvetbaldmime/projects/heappie/" + filename
  )

  val byteBuffer = buf.readAllBytes
  val bv         = ByteVector(byteBuffer.map(_.toByte))

  val result = codecs.heapDump.as[HeapProfile].decode(bv.bits).require.value
  println(
    result.records
      .map(_.tag)
      .groupBy(identity)
      .transform((a, b) => b.length)
  )
  val invariants = Invariants(result)

  assert(invariants.allLoadedClassesHaveAName)
end analyse

class Invariants(heap: HeapProfile):
  import RecordData.*

  lazy val stringsMap = heap.records
    .filter(_.tag == Tag.String)
    .map(_.data)
    .collect { case Strings(id, value) =>
      Try(new String(value.toArray)).toOption.map { str =>
        id.id -> str
      }
    }
    .flatten
    .toMap

  def allLoadedClassesHaveAName =
    heap.records
      .map(_.data)
      .collect { case lc: LoadClass =>
        lc
      }
      .forall(lc => stringsMap.get(lc.classNameId.id).isDefined)
end Invariants
