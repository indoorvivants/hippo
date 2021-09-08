package hippo.backend

import hippo.shared.profile.*
import cats.effect.*

trait HeapExplorerService:
  def getString(sid: StringId): IO[String]
  def getLoadedClass(id: ClassId): IO[Option[String]]

object HeapExplorerService:
  class Impl(profile: HeapProfile) extends HeapExplorerService:
    lazy val stringMap = profile.records.collect {
      case Record(_, _, _, RecordData.Strings(id, StringData.Valid(s))) =>
        id -> s
    }.toMap

    override def getString(sid: StringId) =
      IO.fromOption(stringMap.get(sid))(Err.StringNotFound(sid))
    override def getLoadedClass(id: ClassId) = ???

sealed trait Err
object Err:
  case class StringNotFound(sid: StringId)
      extends Exception(s"String with $sid not found"), Err
