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

object codecs:
  def fixedString(s: String): Codec[Unit] =
    val byteBuffer = s.getBytes
    constant(ByteVector(byteBuffer))

  val nullByte = constant(BitVector.low(8))

  val u1 = uint8
  val u2 = int16
  val u4 = int32
  val u8 = int64

  val header =
    (fixedString("JAVA PROFILE 1.0.") ~>
      ("profile version" | discriminated[ProfileVersion]
        .by(u1)
        .typecase('1'.toByte, provide(ProfileVersion.V1))
        .typecase('2'.toByte, provide(ProfileVersion.V2))) ::
      nullByte ~>
      u4.as[Size.Identifiers] ::
      long(TimeStamp)).as[Metadata]

  def identifier(using sz: Size.Identifiers) =
    "identifier" | (u4 :: u4).as[Identifier]

  def id[A](companion: OpaqueId[A])(using Size.Identifiers): Codec[A] =
    s"identifier ${companion.toString}" | identifier.xmap(
      companion.from(_),
      companion.to(_)
    )

  def int[A](companion: OpaqueIntegral[A, Int]): Codec[A] =
    u4.xmap(companion.from(_), companion.to(_))

  def long[A](companion: OpaqueIntegral[A, Long]): Codec[A] =
    scodec.codecs.int64.xmap(companion.from(_), companion.to(_))

  def float[A](companion: OpaqueIntegral[A, Float]): Codec[A] =
    scodec.codecs.float.xmap(companion.from(_), companion.to(_))

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

  val basicType = u1.xmap(BasicType.fromInt, BasicType.toInt)

  def constantPool(using Size.Identifiers) =
    (int(ConstantPoolIndex) :: basicType)
      .flatAppend(a => value(a._2))
      .as[ConstantPool]

  def instanceField(using Size.Identifiers) =
    (id(FieldId) :: basicType).as[InstanceField]

  def staticField(using Size.Identifiers) =
    (id(FieldId) :: basicType).flatAppend(a => value(a._2)).as[StaticField]

  val instanceSize = u4

  def length(label: String)     = (label | u2)
  def longLength(label: String) = (label | u4)

  def classDump(using Size.Identifiers) =
    (id(ClassId) ::
      int(StackTraceSerialNumber) ::
      id(ClassId) ::
      id(ClassLoaderId) ::
      id(SignersObjectId) ::
      id(ProtectionDomainId) ::
      identifier ::
      identifier ::
      instanceSize ::
      (listOfN(length("constant pool elements"), constantPool)) ::
      (listOfN(length("static fields"), staticField)) ::
      (listOfN(length("instance fields "), instanceField)))
      .as[HeapData.ClassDump]

  def instanceDump(using Size.Identifiers) =
    (id(ObjectId) ::
      int(StackTraceSerialNumber) ::
      id(ClassId) ::
      variableSizeBytes(
        longLength("instance values"),
        bytes
      )).as[HeapData.InstanceDump]

  def objectArrayDump(using Size.Identifiers) =
    (id(ArrayId) ::
      int(StackTraceSerialNumber) ::
      longLength("instance values") ::
      id(ArrayClassId))
      .flatAppend { case (aid, sel, length, acid) =>
        listOfN(
          provide(length),
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
    (int(ThreadSerialNumber) ::
      id(ThreadId) ::
      int(StackTraceSerialNumber) ::
      id(ThreadNameId) ::
      id(ThreadGroupNameId) ::
      id(ThreadParentGroupNameId))
      .as[RecordData.StartThread]
      .upcast[RecordData]

  def primitiveArrayDump(using Size.Identifiers) =
    (id(ArrayId) ::
      int(StackTraceSerialNumber) ::
      longLength("num elements") ::
      basicType)
      .flatAppend { case (_, _, len, typ) =>
        bytes(len * size(typ))
      }
      .as[HeapData.PrimitiveArrayDump]

  def rootThreadObject(using Size.Identifiers) =
    (id(ThreadId) ::
      int(ThreadSerialNumber) ::
      int(StackTraceSerialNumber))
      .as[HeapData.RootThreadObject]

  def rootJavaFrame(using Size.Identifiers) =
    (id(ObjectId) ::
      int(ThreadSerialNumber) ::
      frameInfo).as[HeapData.RootJavaFrame]

  def rootJniLocal(using Size.Identifiers) =
    (id(ObjectId) ::
      int(ThreadSerialNumber) ::
      frameInfo).as[HeapData.RootJniLocal]

  def rootJniGlobal(using Size.Identifiers) =
    (id(ObjectId) ::
      id(JniGlobalRefId)).as[HeapData.RootJniGlobal]

  def rootUnknown(using Size.Identifiers) =
    id(ObjectId).as[HeapData.RootUnknown]

  def rootStickyClass(using Size.Identifiers) =
    id(ObjectId).as[HeapData.RootStickyClass]

  def rootNativeStack(using Size.Identifiers) =
    (id(ObjectId) :: int(ThreadSerialNumber)).as[HeapData.RootNativeStack]

  def rootThreadBlock(using Size.Identifiers) =
    (id(ObjectId) :: int(ThreadSerialNumber)).as[HeapData.RootThreadBlock]

  def rootMonitorUsed(using Size.Identifiers) =
    id(ObjectId).as[HeapData.RootMonitorUsed]

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

  val endThread = int(ThreadSerialNumber)
    .as[RecordData.EndThread]
    .upcast[RecordData]

  val unloadClass = int(ClassSerialNumber)
    .as[RecordData.UnloadClass]
    .upcast[RecordData]

  def loadClass(using Size.Identifiers) =
    (int(ClassSerialNumber) ::
      id(ClassId) ::
      int(StackTraceSerialNumber) ::
      id(ClassNameId))
      .as[RecordData.LoadClass]
      .upcast[RecordData]

  def stackTrace(using Size.Identifiers) =
    (int(StackTraceSerialNumber) ::
      int(ThreadSerialNumber) ::
      listOfN(
        u4,
        id(StackFrameId)
      ))
      .as[RecordData.StackTrace]
      .upcast[RecordData]

  def stackFrame(using Size.Identifiers) =
    (id(StackFrameId) ::
      id(MethodNameId) ::
      id(MethodSignatureId) ::
      id(SourceFileId) ::
      int(ClassSerialNumber) ::
      lineInfo)
      .as[RecordData.StackFrame]
      .upcast[RecordData]

  def stringData(len: Length)(using Size.Identifiers) =
    (id(StringId) :: stringValue(remainingLength(len)))
      .as[RecordData.Strings]
      .upcast[RecordData]

  def heapDataSegment(len: Length)(using Size.Identifiers) =
    val rec = ("list of heap data" | list(heapData))
      .as[RecordData.HeapDumpSegment]
      .upcast[RecordData]

    fixedSizeBytes(len.value, rec)

  def heapDumpEnd(len: Length) =
    fixedSizeBytes(len.value, provide(RecordData.HeapDumpEnd))
      .upcast[RecordData]

  val heapSummary =
    (
      int(TotalLiveBytes) ::
        int(TotalLiveInstances) ::
        long(TotalBytesAllocated) ::
        long(TotalInstancesAllocated)
    ).as[RecordData.HeapSummary]
      .upcast[RecordData]

  val allocationKind = u1.xmap(AllocationKind.fromInt, AllocationKind.toInt)

  val allocationSite =
    (
      allocationKind ::
        int(ClassSerialNumber) ::
        int(StackTraceSerialNumber) ::
        int(TotalLiveBytes) ::
        int(TotalLiveInstances) ::
        int(BytesAllocated) ::
        int(InstancesAllocated)
    ).as[AllocationSite]

  val allocSites =
    (int(Flags) ::
      float(CutoffRatio) ::
      int(TotalLiveBytes) ::
      int(TotalLiveInstances) ::
      long(TotalBytesAllocated) ::
      long(TotalInstancesAllocated) ::
      listOfN(u4, allocationSite))
      .as[RecordData.AllocSites]
      .upcast[RecordData]

  def record(using sz: Size.Identifiers) =
    val preface = tag :: timeShift :: length

    preface.flatAppend {
      case (Tag.String, shift, len)      => stringData(len)
      case (Tag.LoadClass, _, _)         => loadClass
      case (Tag.StackTrace, _, _)        => stackTrace
      case (Tag.StackFrame, _, _)        => stackFrame
      case (Tag.HeapDumpSegment, _, len) => heapDataSegment(len)
      case (Tag.HeapDumpEnd, _, len)     => heapDumpEnd(len)
      case (Tag.UnloadClass, _, _)       => unloadClass
      case (Tag.HeapSummary, _, _)       => heapSummary
      case (Tag.AllocSites, _, _)        => allocSites
      case (Tag.StartThread, _, _)       => startThread
      case (Tag.EndThread, _, _)         => endThread

    }.as[Record]
  end record

  val heapDump = header.flatZip { m =>
    given Size.Identifiers = m.identifiersSize

    vector(record)
  }
end codecs

extension [A](codec: Codec[A])
  private[heappie] def logAs(prefix: String) = logToStdOut(codec, prefix)
