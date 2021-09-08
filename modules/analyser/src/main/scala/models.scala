package heappie

import scodec.bits.ByteVector

enum ProfileVersion:
  case V1, V2

enum Size:
  case Identifiers(sz: Int)

enum Tag:
  case String, LoadClass, UnloadClass, StackFrame, StackTrace, AllocSites,
  HeapSummary, StartThread, EndThread, HeapDumpSegment, HeapDumpEnd

opaque type TimeStamp = Long
object TimeStamp extends heappie.OpaqueIntegral[TimeStamp, Long]

case class Metadata(
    version: ProfileVersion,
    identifiersSize: Size.Identifiers,
    timestamp: TimeStamp
)

case class Identifier(l: Int, r: Int)

opaque type ClassSerialNumber = Int
object ClassSerialNumber extends heappie.OpaqueIntegral[ClassSerialNumber, Int]
opaque type ConstantPoolIndex = Int
object ConstantPoolIndex extends heappie.OpaqueIntegral[ConstantPoolIndex, Int]
opaque type StackTraceSerialNumber = Int
object StackTraceSerialNumber
    extends heappie.OpaqueIntegral[StackTraceSerialNumber, Int]
opaque type ThreadSerialNumber = Int
object ThreadSerialNumber
    extends heappie.OpaqueIntegral[ThreadSerialNumber, Int]

case class TimeShift(shift: Int)
case class Length(value: Int)

abstract class OpaqueId[A](using inv: A =:= Identifier):
  extension (d: A) def id            = inv.apply(d)
  inline def from(id: Identifier): A = inv.flip.apply(id)
  inline def to(id: A): Identifier   = inv.apply(id)

opaque type ThreadNameId = Identifier
object ThreadNameId extends OpaqueId[ThreadNameId]

opaque type ThreadGroupNameId = Identifier
object ThreadGroupNameId extends OpaqueId[ThreadGroupNameId]

opaque type ThreadParentGroupNameId = Identifier
object ThreadParentGroupNameId extends OpaqueId[ThreadParentGroupNameId]

opaque type ThreadId = Identifier
object ThreadId extends OpaqueId[ThreadId]

opaque type ArrayId = Identifier
object ArrayId extends OpaqueId[ArrayId]

opaque type StringId = Identifier
object StringId extends OpaqueId[StringId]

opaque type ObjectId = Identifier
object ObjectId extends OpaqueId[ObjectId]

opaque type ClassId = Identifier
object ClassId extends OpaqueId[ClassId]

opaque type ClassLoaderId = Identifier
object ClassLoaderId extends OpaqueId[ClassLoaderId]

opaque type ClassNameId = Identifier
object ClassNameId extends OpaqueId[ClassNameId]

opaque type ArrayClassId = Identifier
object ArrayClassId extends OpaqueId[ArrayClassId]

opaque type FieldId = Identifier
object FieldId extends OpaqueId[FieldId]

opaque type MethodNameId = Identifier
object MethodNameId extends OpaqueId[MethodNameId]

opaque type StackFrameId = Identifier
object StackFrameId extends OpaqueId[StackFrameId]

opaque type SignersObjectId = Identifier
object SignersObjectId extends OpaqueId[SignersObjectId]

opaque type SourceFileId = Identifier
object SourceFileId extends OpaqueId[SourceFileId]

opaque type ProtectionDomainId = Identifier
object ProtectionDomainId extends OpaqueId[ProtectionDomainId]

opaque type JniGlobalRefId = Identifier
object JniGlobalRefId extends OpaqueId[JniGlobalRefId]

opaque type MethodSignatureId = Identifier
object MethodSignatureId extends OpaqueId[MethodSignatureId]

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

enum Value:
  case U1(value: Int)
  case U2(value: Int)
  case U4(value: Int)
  case U8(value: Long)
  case ObjectId(id: Identifier)

enum FrameInfo:
  case Num(i: Int)
  case Empty

object FrameInfo:
  def fromInt(i: Int) = i match
    case -1 => Empty
    case n  => Num(n)

  def toInt(f: FrameInfo) = f match
    case Empty  => -1
    case Num(n) => n

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
      case 10 => Int

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
      case Int     => 10

end BasicType

case class ConstantPool(
    index: ConstantPoolIndex,
    entryType: BasicType,
    value: Value
)

case class StaticField(
    nameId: FieldId,
    fieldType: BasicType,
    value: Value
)

case class InstanceField(
    nameId: FieldId,
    fieldType: BasicType
)

enum HeapData:
  case RootUnknown(objectId: ObjectId)
  case RootJniGlobal(objectId: ObjectId, jniGlobalRefId: JniGlobalRefId)
  case RootJniLocal(
      objectId: ObjectId,
      threadSerialNumber: ThreadSerialNumber,
      frameInfo: FrameInfo
  )
  case RootJavaFrame(
      objectId: ObjectId,
      threadSerialNumber: ThreadSerialNumber,
      frameInfo: FrameInfo
  )
  case RootNativeStack(
      objectId: ObjectId,
      threadSerialNumber: ThreadSerialNumber
  )
  case RootStickyClass(objectId: ObjectId)
  case RootThreadBlock(
      objectId: ObjectId,
      threadSerialNumber: ThreadSerialNumber
  )
  case RootMonitorUsed(
      objectId: ObjectId
  )
  case RootThreadObject(
      threadId: ThreadId,
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

  case InstanceDump(
      objectId: ObjectId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      classId: ClassId,
      values: ByteVector
  )

  case ObjectArrayDump(
      id: ArrayId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      numElements: Int,
      arrayClassId: ArrayClassId,
      elements: List[Identifier]
  )

  case PrimitiveArrayDump(
      id: ArrayId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      numElements: Int,
      elementType: BasicType,
      els: ByteVector
  )
end HeapData

opaque type TotalLiveBytes = Int
object TotalLiveBytes extends OpaqueIntegral[TotalLiveBytes, Int]

opaque type TotalLiveInstances = Int
object TotalLiveInstances extends OpaqueIntegral[TotalLiveInstances, Int]

opaque type TotalBytesAllocated = Long
object TotalBytesAllocated extends OpaqueIntegral[TotalBytesAllocated, Long]

opaque type TotalInstancesAllocated = Long
object TotalInstancesAllocated
    extends OpaqueIntegral[TotalInstancesAllocated, Long]

opaque type BytesAllocated = Int
object BytesAllocated extends OpaqueIntegral[BytesAllocated, Int]

opaque type InstancesAllocated = Int
object InstancesAllocated extends OpaqueIntegral[InstancesAllocated, Int]

opaque type CutoffRatio = Float
object CutoffRatio extends OpaqueIntegral[CutoffRatio, Float]

abstract class OpaqueIntegral[A, I: Numeric](using inv: I =:= A):
  inline def from(i: I)             = inv.apply(i)
  inline def to(o: A)               = inv.flip.apply(o)
  extension (d: A) inline def value = to(d)

opaque type Flags = Int
object Flags extends OpaqueIntegral[Flags, Int]:
  val Incremental: Flags        = 0x1
  val SortedByAllocation: Flags = 0x2
  val ForceGC: Flags            = 0x4

  extension (f: Flags) def is(other: Flags) = (f & other) != 0

enum AllocationKind:
  case Single
  case ArrayOf(typ: BasicType)

object AllocationKind:
  def fromInt(i: Int) =
    i match
      case 0 => Single
      case n => ArrayOf(BasicType.fromInt(n))

  def toInt(a: AllocationKind) =
    a match
      case Single     => 0
      case ArrayOf(n) => BasicType.toInt(n)

case class AllocationSite(
    kind: AllocationKind,
    classSerialNumber: ClassSerialNumber,
    stackTraceSerialNumber: StackTraceSerialNumber,
    liveBytes: TotalLiveBytes,
    liveInstances: TotalLiveInstances,
    bytesAllocated: BytesAllocated,
    instancesAllocated: InstancesAllocated
)

enum RecordData:
  case Strings(id: StringId, content: ByteVector)
  case UnloadClass(classSerialNumber: ClassSerialNumber)
  case EndThread(threadSerialNumber: ThreadSerialNumber)
  case AllocSites(
      flags: Flags,
      cutoffRation: CutoffRatio,
      totalLiveBytes: TotalLiveBytes,
      totalLiveInstances: TotalLiveInstances,
      totalBytesAllocated: TotalBytesAllocated,
      totalInstancesAllocated: TotalInstancesAllocated,
      sites: List[AllocationSite]
  )
  case LoadClass(
      classSerialNumber: ClassSerialNumber,
      classObjectId: ClassId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      classNameId: ClassNameId
  )
  case StackTrace(
      stackTraceSerialNumber: StackTraceSerialNumber,
      threadSerialNumber: ThreadSerialNumber,
      frames: List[StackFrameId]
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
  case HeapDumpEnd
  case HeapSummary(
      totalLiveBytes: TotalLiveBytes,
      totalLiveInstances: TotalLiveInstances,
      totalBytesAllocated: TotalBytesAllocated,
      totalInstancesAllocated: TotalInstancesAllocated
  )
  case StartThread(
      threadSerialNumber: ThreadSerialNumber,
      threadId: ThreadId,
      stackTraceSerialNumber: StackTraceSerialNumber,
      threadNameId: ThreadNameId,
      threadGroupNameId: ThreadGroupNameId,
      threadParentGroupNameId: ThreadParentGroupNameId
  )
  case Unknown
end RecordData

case class HeapProfile(metadata: Metadata, records: Vector[Record])

case class Record(
  tag: Tag,
  shift: TimeShift,
  length: Length,
  data: RecordData
)
