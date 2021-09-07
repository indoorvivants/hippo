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

enum ProfileVersion:
  case V1, V2

enum Size:
  case Identifiers(sz: Int)

enum Tag:
  case String, LoadClass, UnloadClass, StackFrame, StackTrace, AllocSites,
  HeapSummary, StartThread, EndThread, HeapDumpSegment

  def int = this match
    case String => 1

case class TimeStamp(high: Int, low: Int)
case class Metadata(
    version: ProfileVersion,
    identifiersSize: Size.Identifiers,
    timestamp: TimeStamp
)

case class Identifier(l: Int, r: Int)
case class ClassSerialNumber(value: Int)
case class StackTraceSerialNumber(value: Int)
case class ThreadSerialNumber(value: Int)
case class ConstantPoolIndex(value: Int)

case class TimeShift(shift: Int)
case class Length(value: Int)

case class ClassId(id: Identifier)
case class FieldId(id: Identifier)
case class StringId(id: Identifier)
case class ClassNameId(id: Identifier)
case class ClassLoaderId(id: Identifier)
case class SignersObjectId(id: Identifier)
case class ProtectionDomainId(id: Identifier)
case class StackFrameId(id: Identifier)
case class MethodNameId(id: Identifier)
case class MethodSignatureId(id: Identifier)
case class SourceFileId(id: Identifier)

enum LineInformation:
  case Number(i: Int)
  case Empty, CompiledMethod, NativeMethod, Unknown

object LineInformation:
  def fromInt(i: Int) =
    i match
      case 0  => Empty
      case -1 => Unknown
      case -2 => CompiledMethod
      case -3 => NativeMethod
      case n  => Number(n)

  def toInt(value: LineInformation) =
    value match
      case Empty          => 0
      case Unknown        => -1
      case CompiledMethod => -2
      case NativeMethod   => -3
      case Number(n)      => n
end LineInformation

enum FrameInfo:
  case Num(i: Int)
  case Empty

enum BasicType:
  case Object, Boolean, Char, Float, Double, Byte, Short, Int, Long

object BasicType:
  def fromInt(i: Int) =
    i match
      case 2  => Object
      case 4  => Boolean
      case 5  => Char
      case 6  => Float
      case 7  => Double
      case 8  => Byte
      case 9  => Short
      case 11 => Long

  def toInt(value: BasicType) =
    value match
      case Object  => 2
      case Boolean => 4
      case Char    => 5
      case Float   => 6
      case Double  => 7
      case Byte    => 8
      case Short   => 9
      case Long    => 11

end BasicType

case class ConstantPool(
    index: ConstantPoolIndex,
    entryType: BasicType,
    value: Long // TODO
)

case class StaticField(
    nameId: FieldId,
    fieldType: BasicType,
    value: Long // TODO
)

case class InstanceField(
    nameId: FieldId,
    fieldType: BasicType
)

enum HeapData:
  case RootUnknown(objectId: Identifier)
  case RootJniGlobal(objectId: Identifier, jniGlobalRefId: Identifier)
  case RootJniLocal(
      objectId: Identifier,
      threadSerialNumber: ThreadSerialNumber,
      frameInfo: FrameInfo
  )
  case RootJavaFrame(
      objectId: Identifier,
      threadSerialNumber: ThreadSerialNumber,
      frameInfo: FrameInfo
  )
  case RootNativeStack(
      objectId: Identifier,
      threadSerialNumber: ThreadSerialNumber
  )
  case RootStickyClass(objectId: Identifier)
  case RootThreadBlock(
      objectId: Identifier,
      threadSerialNumber: ThreadSerialNumber
  )
  case RootMonitorUsed(
      objectId: Identifier
  )
  case RootThreadObject(
      threadId: Identifier,
      threadSerialNumber: ThreadSerialNumber,
      stackTraceSerialNumber: StackTraceSerialNumber
  )
  case ClassDump(
      classId: ClassId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      superClassId: ClassId,
      classLoaderId: ClassLoaderId,
      signersId: SignersObjectId,
      protectionDomainId: ProtectionDomainId,
      reserved1: Identifier,
      reserved2: Identifier,
      instanceSize: Int,
      constantPools: List[ConstantPool],
      staticFields: List[StaticField],
      instanceFields: List[InstanceField]
  )
end HeapData

enum RecordData:
  case Strings(id: StringId, content: ByteVector)
  case LoadClass(
      classSerialNumber: ClassSerialNumber,
      classObjectId: ClassId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      classNameId: ClassNameId
  )
  case StackTrace(
      stackTraceSerialNumber: StackTraceSerialNumber,
      threadSerialNumber: ThreadSerialNumber,
      frames: List[Identifier]
  )
  case StackFrame(
      stackFrame: StackFrameId,
      methodNameId: MethodNameId,
      methodSignatureId: MethodSignatureId,
      sourceFileId: SourceFileId,
      classNumberId: ClassSerialNumber,
      lineNumber: LineInformation
  )
  case HeapDumpSegment(roots: List[HeapData])
  case HeapDump(segments: List[HeapDumpSegment])
  case Unknown
end RecordData

object Codecs:
  def fixedString(s: String): Codec[Unit] =
    val byteBuffer = s.getBytes
    constant(ByteVector(byteBuffer))

  val nullByte = constant(BitVector.low(8))

  val u1 = uint8
  val u4 = int32
  val u2 = int16
  val u8 = u4 :: u4

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
  def classId(using Size.Identifiers)  = "class id" | identifier.as[ClassId]
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
  end tag

  val timeShift = "time_shift" | u4.as[TimeShift]
  val length    = "length" | u4.as[Length]
  val lineInfo  = u4.xmap(LineInformation.fromInt, LineInformation.toInt)
  val stackTraceSerialNumber = u4.as[StackTraceSerialNumber]
  val threadSerialNumber     = u4.as[ThreadSerialNumber]
  val classSerialNumber      = u4.as[ClassSerialNumber]

  val basicType = u1.xmap(BasicType.fromInt, BasicType.toInt)

  val value = "field value" | uint32

  val constantPool =
    (u2.as[ConstantPoolIndex] :: basicType :: value).as[ConstantPool]

  def instanceField(using Size.Identifiers) =
    (fieldId :: basicType).as[InstanceField]

  def staticField(using Size.Identifiers) =
    (fieldId :: basicType :: value).as[StaticField]

  val instanceSize = u4

  def classDump(using Size.Identifiers) =
    (
      classId ::
        stackTraceSerialNumber ::
        classId ::
        classLoaderId ::
        signersObjectId ::
        protectionDomainId ::
        identifier ::
        identifier ::
        instanceSize ::
        ("constant pool elements" | listOfN(u2, constantPool)) ::
        ("static fields" | listOfN(u2, staticField)) ::
        ("instance fields " | listOfN(u2, instanceField))
    ).as[HeapData.ClassDump]

  def heapData(using Size.Identifiers) =
    discriminated[HeapData]
      .by(u1)
      .typecase(0x20, logToStdOut("class dump" | classDump))

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

      case (Tag.HeapDumpSegment, _, _) =>
        ("list of heap data" | list(heapData))
          .as[RecordData.HeapDumpSegment]

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

  given Size.Identifiers = Size.Identifiers(8)

  val heapDump = header.flatZip { meta =>
    given Size.Identifiers = meta.identifiersSize

    vector(record.decodeOnly)
  }

  println(heapDump.decode(bv.bits))

end analyse

// extension [A](codec: Codec[A])
//   def decodeButMoreAnnoying(bv: ByteVector) =
//     val original = codec.decode(bv.toBitVector)

//     original match
//       case Attempt.Successful(res) => res
//       case Attempt.Failure(err)    => throw new Exception(err.toString)

// end extension

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
