package hippo.backend

import hippo.shared.profile.*
import cats.effect.*
import hippo.shared.profile.RecordData.Strings
import HeapData as hd
import RecordData as rd
import StringData as sd
import io.circe.Codec

trait HeapExplorerService:
  def getString(sid: StringId): IO[StringData]
  def getPrimitiveArray(aid: ArrayId): IO[hd.PrimitiveArrayDump]
  def getObjectArray(aid: ArrayId): IO[hd.ObjectArrayDump]
  def getLoadedClass(id: ClassId): IO[Option[String]]
  def stringByPrefix(search: String): IO[List[RecordData]]
  def getSummary: IO[Summary]

object HeapExplorerService:
  class Impl(profile: HeapProfile) extends HeapExplorerService:
    val views = Views(profile)
    import views.*

    arrayMap.toIterator.collect {
      case (id, _: hd.PrimitiveArrayDump) => println(s"Primitive array: $id")
    }.take(5).toList
    
    arrayMap.toIterator.collect {
      case (id, _: hd.ObjectArrayDump) => println(s"Object array: $id")
    }.take(5).toList

    override def getString(sid: StringId) =
      IO.fromOption(stringMap.get(sid).map(_.content))(Err.StringNotFound(sid))

    override def getLoadedClass(id: ClassId) = ???

    override def stringByPrefix(search: String) =
      IO(validStrings.filter(_._1.startsWith(search)).map(_._2).toList)

    override def getPrimitiveArray(aid: ArrayId) =
      IO.fromOption(arrayMap.get(aid).collect { case p: hd.PrimitiveArrayDump =>
        p
      })(Err.ArrayNotFound(aid))

    override def getObjectArray(aid: ArrayId) =
      IO.fromOption(arrayMap.get(aid).collect { case p: hd.ObjectArrayDump =>
        p
      })(Err.ArrayNotFound(aid))

    override def getSummary =
      IO(views.heapDataByType)
        .product(IO(views.segmentTypes.toList.flatMap(_.toList)))
        .map(Summary.apply)
  end Impl
end HeapExplorerService

sealed trait Err
object Err:
  case class StringNotFound(sid: StringId)
      extends Exception(s"String with $sid not found"),
        Err
  case class ArrayNotFound(aid: ArrayId)
      extends Exception(s"Array with $aid not found"),
        Err
