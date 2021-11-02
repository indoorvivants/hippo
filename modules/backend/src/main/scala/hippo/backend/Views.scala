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
  import RecordData as rd
  import StringData as sd
  val records = profile.records.map(_.data)

  lazy val stringMap: Map[StringId, Strings] = profile.records.collect {
    case Record(_, _, _, s @ rd.Strings(id, _)) =>
      id -> s
  }.toMap

  lazy val validStrings: Map[String, Strings] = stringMap.collect {
    case (id, rec @ RecordData.Strings(_, sd.Valid(data))) =>
      data -> rec
  }

  lazy val invalidStrings = stringMap.collect {
    case (id, rec @ RecordData.Strings(_, sd.Invalid(data))) =>
      id -> data
  }

  lazy val loadedClasses = profile.records.collect {
    case Record(_, _, _, s: rd.LoadClass) =>
      s.classNameId.as(StringId) -> s
  }

  lazy val segmentTypes =
    profile.records
      .find(_.tag == Tag.HeapDumpSegment)
      .collect { case Record(_, _, _, rd.HeapDumpSegment(seg)) =>
        import scala.util.chaining.*
        seg
          .groupBy(_.getClass.getSimpleName)
          .mapValues(_.size)
          .toMap
          .tap(println)
      }

  lazy val heapDataByType =
    profile.records
      .map(_.tag)
      .groupBy(identity)
      .transform((_, v) => v.size)
      .toList

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

  import HeapData.*

  lazy val arrayMap: Map[ArrayId, PrimitiveArrayDump | ObjectArrayDump] =
    heapData.collect {
      case pd: HeapData.PrimitiveArrayDump =>
        pd.id -> pd
      case hd: HeapData.ObjectArrayDump =>
        hd.id -> hd
    }.toMap
end Views

object Views:
  def apply(prof: HeapProfile) = new Views(prof)
