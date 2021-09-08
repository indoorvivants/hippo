package heappie

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.io.FileReader
import java.io.File
import scodec.bits.*
import scodec.*
import scodec.codecs.*
import scala.util.Using
import java.io.ByteArrayInputStream
import java.io.FileInputStream
import java.lang.annotation.Native
import scala.util.Try

object Codecs:
  def fixedString(s: String): Codec[Unit] =
    val byteBuffer = s.getBytes
    constant(ByteVector(byteBuffer))

  val nullByte = constant(BitVector.low(8))

  val u1 = uint8
  val u4 = int32
  val u2 = int16
  val u8 = ("id high" | u4) :: ("id low" | u4)

  val header =
    (fixedString("JAVA PROFILE 1.0.") ~>
      ("profile version" | discriminated[ProfileVersion]
        .by(u1)
        .typecase('1'.toByte, provide(ProfileVersion.V1))
        .typecase('2'.toByte, provide(ProfileVersion.V2))) ::
      nullByte ~>
      u4.as[Size.Identifiers] ::
      u8.as[TimeStamp]).as[Metadata]

  def identifier(using sz: Size.Identifiers) = "identifier" | u8.as[Identifier]
  def id[A](companion: OpaqueId[A])(using Size.Identifiers): Codec[A] =
    s"identifier ${companion.toString}" | identifier.xmap(
      companion.from(_),
      companion.to(_)
    )

  def int[A](companion: OpaqueIntegral[A, Int]): Codec[A] =
    u4.xmap(companion.from(_), companion.to(_))

  def classId(using Size.Identifiers) = "class id" | identifier.as[ClassId]
  def arrayId(using Size.Identifiers) = "array id" | identifier.as[ArrayId]
  def arrayClassId(using Size.Identifiers) =
    "array class id" | identifier.as[ArrayClassId]
  def objectId(using Size.Identifiers) = "object id" | identifier.as[ObjectId]
  def threadId(using Size.Identifiers) = "object id" | identifier.as[ThreadId]
  def jniGlobalRefId(using Size.Identifiers) =
    "object id" | identifier.as[JniGlobalRefId]
  def stringId(using Size.Identifiers) = "string id" | identifier.as[StringId]
  def classNameId(using Size.Identifiers) =
    "classname id" | identifier.as[ClassNameId]
  def classLoaderId(using Size.Identifiers) =
    "classloader id" | identifier.as[ClassLoaderId]
  def signersObjectId(using Size.Identifiers) = identifier.as[SignersObjectId]
  def protectionDomainId(using Size.Identifiers) =
    identifier.as[ProtectionDomainId]
  def stackFrameId(using Size.Identifiers) =
    identifier.as[StackFrameId]
  def methodNameId(using Size.Identifiers) =
    identifier.as[MethodNameId]
  def methodSignatureId(using Size.Identifiers) =
    identifier.as[MethodSignatureId]
  def sourceFileId(using Size.Identifiers) =
    identifier.as[SourceFileId]
  def fieldId(using Size.Identifiers) =
    identifier.as[FieldId]
  def remainingLength(len: Length)(using id: Size.Identifiers) =
    len.value - id.sz

  def stringValue(len: Int)(using Size.Identifiers) =
    "string content" | bytes(len)

  def value(of: BasicType)(using Size.Identifiers) =
    import BasicType as bt
    of match
      case bt.Long | bt.Double  => u8.as[Value.U8].upcast[Value]
      case bt.Int | bt.Float    => u4.as[Value.U4].upcast[Value]
      case bt.Boolean | bt.Byte => u1.as[Value.U1].upcast[Value]
      case bt.Object            => identifier.as[Value.ObjectId].upcast[Value]
      case _                    => u2.as[Value.U2].upcast[Value]

  val tag =
    import Tag.*
    "tag" | discriminated[Tag]
      .by(u1)
      .typecase(0x01, provide(Tag.String))
      .typecase(0x02, provide(Tag.LoadClass))
      .typecase(0x03, provide(Tag.UnloadClass))
      .typecase(0x04, provide(Tag.StackFrame))
      .typecase(0x05, provide(Tag.StackTrace))
      .typecase(0x06, provide(Tag.AllocSites))
      .typecase(0x07, provide(Tag.HeapSummary))
      .typecase(0x0a, provide(Tag.StartThread))
      .typecase(0x0b, provide(Tag.EndThread))
      .typecase(0x0c, provide(Tag.HeapDumpSegment))
      .typecase(0x1c, provide(Tag.HeapDumpSegment))
      .typecase(0x2c, provide(Tag.HeapDumpEnd))
  end tag

  val timeShift = "time_shift" | u4.as[TimeShift]
  val length    = "length" | u4.as[Length]
  val lineInfo  = u4.xmap(LineInformation.fromInt, LineInformation.toInt)
  val stackTraceSerialNumber =
    "stack trace serial number" | u4.as[StackTraceSerialNumber]
  val threadSerialNumber = u4.as[ThreadSerialNumber]
  val classSerialNumber  = u4.as[ClassSerialNumber]

  val basicType = u1.xmap(BasicType.fromInt, BasicType.toInt)

  val constantPoolIndex = u2.as[ConstantPoolIndex]

  def constantPool(using Size.Identifiers) =
    (constantPoolIndex :: basicType)
      .flatAppend(a => value(a._2))
      .as[ConstantPool]

  def instanceField(using Size.Identifiers) =
    (fieldId :: basicType).as[InstanceField]

  def staticField(using Size.Identifiers) =
    (fieldId :: basicType).flatAppend(a => value(a._2)).as[StaticField]

  val instanceSize = u4

  def length(label: String)     = (label | u2)
  def longLength(label: String) = (label | u4)

  def classDump(using Size.Identifiers) =
    (classId ::
      stackTraceSerialNumber ::
      classId ::
      classLoaderId ::
      signersObjectId ::
      protectionDomainId ::
      identifier ::
      identifier ::
      instanceSize ::
      (listOfN(length("constant pool elements"), constantPool)) ::
      (listOfN(length("static fields"), staticField)) ::
      (listOfN(length("instance fields "), instanceField)))
      .as[HeapData.ClassDump]

  def instanceDump(using Size.Identifiers) =
    (objectId ::
      stackTraceSerialNumber ::
      classId ::
      variableSizeBytes(
        longLength("instance values"),
        bytes
      )).as[HeapData.InstanceDump]

  def objectArrayDump(using Size.Identifiers) =
    (arrayId ::
      stackTraceSerialNumber ::
      longLength("instance values") ::
      arrayClassId)
      .flatAppend { case (aid, sel, length, acid) =>
        listOfN(
          provide(length), // what
          identifier
        )
      }
      .as[HeapData.ObjectArrayDump]

  def size(bt: BasicType)(using Size.Identifiers) =
    import BasicType as t
    bt match
      case t.Boolean | t.Byte => 1
      case t.Object           => summon[Size.Identifiers].sz
      case t.Long | t.Double  => 8
      case t.Short | t.Char   => 2
      case t.Float | t.Int    => 4

  val frameInfo = u4.xmap(FrameInfo.fromInt, FrameInfo.toInt)

  def startThread(using Size.Identifiers) =
    (threadSerialNumber ::
      threadId ::
      stackTraceSerialNumber ::
      id(ThreadNameId) ::
      id(ThreadGroupNameId) ::
      id(ThreadParentGroupNameId)).as[RecordData.StartThread]

  def primitiveArrayDump(using Size.Identifiers) =
    (arrayId ::
      stackTraceSerialNumber ::
      longLength("num elements") ::
      basicType)
      .flatAppend { case (_, _, len, typ) =>
        bytes(len * size(typ))
      }
      .as[HeapData.PrimitiveArrayDump]

  def rootThreadObject(using Size.Identifiers) =
    (identifier.as[ThreadId] ::
      threadSerialNumber ::
      stackTraceSerialNumber)
      .as[HeapData.RootThreadObject]
      .logAs("root")

  def rootJavaFrame(using Size.Identifiers) =
    (objectId ::
      threadSerialNumber ::
      frameInfo).as[HeapData.RootJavaFrame]

  def rootJniLocal(using Size.Identifiers) =
    (objectId ::
      threadSerialNumber ::
      frameInfo).as[HeapData.RootJniLocal]

  def rootJniGlobal(using Size.Identifiers) =
    (objectId ::
      jniGlobalRefId).as[HeapData.RootJniGlobal]

  def rootUnknown(using Size.Identifiers) =
    objectId.as[HeapData.RootUnknown]

  def rootStickyClass(using Size.Identifiers) =
    objectId.as[HeapData.RootStickyClass]

  def rootNativeStack(using Size.Identifiers) =
    (objectId :: threadSerialNumber).as[HeapData.RootNativeStack]

  def rootThreadBlock(using Size.Identifiers) =
    (objectId :: threadSerialNumber).as[HeapData.RootThreadBlock]

  def rootMonitorUsed(using Size.Identifiers) =
    objectId.as[HeapData.RootMonitorUsed]

  def heapData(using Size.Identifiers) =
    discriminated[HeapData]
      .by(u1)
      .typecase(0x20, "class dump" | classDump)
      .typecase(0x21, "instance dump" | instanceDump)
      .typecase(0x22, "object array dump" | objectArrayDump)
      .typecase(
        0x23,
        "primitive array dump" | primitiveArrayDump
      )
      .typecase(0x08, "root thread object" | rootThreadObject)
      .typecase(0x03, "root java frame" | rootJavaFrame)
      .typecase(0x02, "root jni local" | rootJniLocal)
      .typecase(0x01, "root jni global" | rootJniGlobal)
      .typecase(0x05, "root sticky class" | rootStickyClass)
      .typecase(0xff, "root unknown" | rootUnknown)
      .typecase(0x04, "root native stack" | rootNativeStack)
      .typecase(0x06, "root thread block" | rootThreadBlock)
      .typecase(0x07, "root monitor used" | rootMonitorUsed)

  def record(using sz: Size.Identifiers) =
    val preface = tag :: timeShift :: length

    preface.flatMap {
      case (Tag.String, shift, len) =>
        (stringId :: stringValue(remainingLength(len)))
          .as[RecordData.Strings]
          .upcast[RecordData]

      case (Tag.LoadClass, _, _) =>
        (classSerialNumber :: classId :: stackTraceSerialNumber :: classNameId)
          .as[RecordData.LoadClass]
          .upcast[RecordData]

      case (Tag.StackTrace, _, _) =>
        (stackTraceSerialNumber :: threadSerialNumber :: listOfN(
          u4,
          identifier
        ))
          .as[RecordData.StackTrace]
          .upcast[RecordData]

      case (Tag.StackFrame, _, _) =>
        (stackFrameId :: methodNameId :: methodSignatureId :: sourceFileId :: classSerialNumber :: lineInfo)
          .as[RecordData.StackFrame]
          .upcast[RecordData]

      case (Tag.HeapDumpSegment, _, len) =>
        val rec = ("list of heap data" | list(heapData))
          .as[RecordData.HeapDumpSegment]

        fixedSizeBytes(len.value, rec)
      case (Tag.HeapDumpEnd, _, len) =>
        fixedSizeBytes(len.value, provide(RecordData.HeapDumpEnd))

    }
  end record
end Codecs

@main def analyse =
  val buf = new FileInputStream(
    "/Users/velvetbaldmime/projects/heappie/heapdump-1631032668990.hprof"
  )

  val byteBuffer = buf.readAllBytes
  val bv         = ByteVector(byteBuffer.map(_.toByte))
  import Codecs.*

  val heapDump = header.flatZip { meta =>
    given Size.Identifiers = meta.identifiersSize

    vector(record.decodeOnly)
  }

  val result     = heapDump.as[HeapProfile].decode(bv.bits).require.value
  val invariants = Invariants(result)
  assert(invariants.allLoadedClassesHaveAName)
end analyse

class Invariants(heap: HeapProfile):
  import RecordData.*
  lazy val stringsMap = heap.records
    .collect { case Strings(id, value) =>
      Try(new String(value.toArray)).toOption.map { str =>
        id.id -> str
      }
    }
    .flatten
    .toMap

  def allLoadedClassesHaveAName =
    heap.records
      .collect { case lc: LoadClass =>
        lc
      }
      .forall(lc => stringsMap.get(lc.classNameId.id).isDefined)
end Invariants

extension [A](codec: Codec[A])
  def logAs(prefix: String) = logToStdOut(codec, prefix)

// @main def forFucksSake =
//   val nice = """
//   // some prelude
//   4a 41 56 41 20 50 52 4f 46 49 4c 45 20 31
//   2e 30 2e 32 00 00 00 00 08 00 00 01 7b c1 1f
//   76 84

//   01 //tag
//     00 00 00 00 //timeshift
//     00 00 00 29 //length
//     00 00 00 01 31 0e a7 40 //identifier
//     24 61 6e 6f 6e 66 75 6e 24 74 72 61 76 65 72 73 65 46 69 6c 74 65 72 56 69 61 43 68 61 69 6e 24 34 // utf8 string

// 01 //tag
//     00 00 00 00 //timeshift
//     00 00 00 23 //length
//     00 00 00 01 2f 87 d6 b0 //identifier
//     63 61 74 73 2f 53 68 6f 77 24 53 68 6f 77 49 6e 74 65 72 70 6f 6c 61 74 6f 72 24 //utf8 string
//   """

//   val ugly =
//     "//.*".r.replaceAllIn(nice, "").replace(" ", "").replace("\n", "")
//   import Codecs.*

//   given Size.Identifiers = Size.Identifiers(8)

//   val buf = new FileInputStream(
//     "/Users/velvetbaldmime/projects/heappie/heapdump-1631032668990.hprof"
//   )

//   val viaIO = buf.readAllBytes.take(150)

//   val f = new FileReader(
//     new File(
//       "/Users/velvetbaldmime/projects/heappie/heapdump-1631032668990.hprof"
//     )
//   )

//   val bf = new Array[Char](150)

//   f.read(bf)

//   val viaStupidShit = bf.map(_.toByte)

//   viaStupidShit.zip(viaIO).zipWithIndex.foreach { case ((wrong, right), idx) =>
//     if wrong != right then
//       println(s"discrepancy($wrong != $right) at index $idx")
//   }
// // println(
// //   (header :: record :: record)
// //     // .decode(ByteVector.fromHex(ugly).get.bits)
// //     .decode(ByteVector(bytes).bits)
// //     .require
// // )

// end forFucksSake
