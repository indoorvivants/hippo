package hippo.analyse

import java.io.*
import scodec.bits.ByteVector
import scala.util.Try
import io.circe.syntax.*

import hippo.shared.profile.*

object Analyser:
  def analyse(bv: ByteVector): HeapProfile =
    val value = codecs.heapDump.as[HeapProfile].decode(bv.bits).require.value
    println("Some examples")

    value.records.take(5).foreach(println)
    value
    
