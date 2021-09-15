package hippo.backend

import hippo.shared.profile.*
import cats.effect.*
import hippo.shared.profile.RecordData.Strings

trait HeapExplorerService:
  def getString(sid: StringId): IO[StringData]
  def getLoadedClass(id: ClassId): IO[Option[String]]
  def stringByPrefix(search: String): IO[List[RecordData]]

object HeapExplorerService:
  import RecordData as rd
  import StringData as sd
  class Impl(profile: HeapProfile) extends HeapExplorerService:
    lazy val stringMap: Map[StringId, Strings] = profile.records.collect {
      case Record(_, _, _, s @ rd.Strings(id, _)) =>
        id -> s
    }.toMap

    lazy val validStrings: Map[String, Strings] = stringMap.collect {
      case (id, rec @ RecordData.Strings(_, sd.Valid(data))) =>
        data -> rec
    }

    override def getString(sid: StringId) =
      IO.fromOption(stringMap.get(sid).map(_.content))(Err.StringNotFound(sid))

    override def getLoadedClass(id: ClassId) = ???
    override def stringByPrefix(search: String) =
      IO(validStrings.filter(_._1.startsWith(search)).map(_._2).toList)
  end Impl
end HeapExplorerService

sealed trait Err
object Err:
  case class StringNotFound(sid: StringId)
      extends Exception(s"String with $sid not found"),
        Err
