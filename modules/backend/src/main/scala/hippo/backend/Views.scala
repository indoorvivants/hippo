package hippo.backend

import io.circe.Codec
import hippo.shared.profile.*

case class LoadedClass(
    name: String,
    stackTrace: LoadedStackTrace
) derives Codec.AsObject

case class LoadedStackTrace(
    threadName: String
) derives Codec.AsObject

case class StackFrame(
    methodName: String,
    sourceFileName: String,
    className: String,
    lineNumber: LineInformation
) derives Codec.AsObject

class Views private (profile: HeapProfile):
  import RecordData.*
  val records = profile.records.map(_.data)

  lazy val strings: Map[StringId, String] = records.collect {
    case Strings(id, StringData.Valid(s)) => id -> s
  }.toMap

  lazy val heapData = records.collect { case hd: HeapDumpSegment =>
    hd.roots
  }.flatten

  lazy val hdTypes =
    heapData.groupBy(_.getClass.getSimpleName).foreach { case (s, samples) =>
      println(samples.take(5))
    }

  lazy val threadMap = heapData.collect {
    case HeapData.RootThreadObject(tid, _, stsn) => 
      println(s"Thread: ${strings.get(tid.as(StringId))}")
  }
end Views

object Views:
  def apply(prof: HeapProfile) = new Views(prof)
