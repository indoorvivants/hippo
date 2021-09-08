package heappie

import scodec.bits.ByteVector

enum ProfileVersion:
  case V1, V2

enum Size:
  case Identifiers(sz: Int)

enum Tag:
  case String, LoadClass, UnloadClass, StackFrame, StackTrace, AllocSites,
  HeapSummary, StartThread, EndThread, HeapDumpSegment, HeapDumpEnd

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

abstract class OpaqueId[A](using inv: A =:= Identifier) {
  extension (d: A) def id = inv.apply(d)
  inline def from(id: Identifier): A = inv.flip.apply(id)
  inline def to(id: A): Identifier = inv.apply(id)
}

opaque type ThreadNameId = Identifier
object ThreadNameId extends OpaqueId[ThreadNameId]

opaque type ThreadGroupNameId = Identifier
object ThreadGroupNameId extends OpaqueId[ThreadGroupNameId]

opaque type ThreadParentGroupNameId = Identifier
object ThreadParentGroupNameId extends OpaqueId[ThreadParentGroupNameId]

case class ClassId(id: Identifier)
case class JniGlobalRefId(id: Identifier)
case class ThreadId(id: Identifier)
case class ArrayId(id: Identifier)
case class ArrayClassId(id: Identifier)
case class ObjectId(id: Identifier)
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

enum Value:
  case U1(value: Int)
  case U2(value: Int)
  case U4(value: Int)
  case U8(value: (Int, Int))
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
object TotalInstancesAllocated extends OpaqueIntegral[TotalBytesAllocated, Long]

abstract class OpaqueIntegral[A, I: Numeric](using inv: I =:= A):
  inline def from(i: I)          = inv.apply(i)
  inline def to(o: A)            = inv.flip.apply(o)
  extension (d: A) inline def value = to(d)

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

case class HeapProfile(metadata: Metadata, records: Vector[RecordData])
