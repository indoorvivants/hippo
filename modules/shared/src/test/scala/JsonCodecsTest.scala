package hippo.shared
import hippo.shared.profile.*
import io.circe.Decoder
import cats.effect.IO
import io.circe.Encoder
import weaver.scalacheck.CheckConfig

object JsonCodecsTest
    extends weaver.SimpleIOSuite
    with weaver.scalacheck.Checkers:

  override def checkConfig = CheckConfig.default.copy(minimumSuccessful = 500)

  def parse[A: Decoder](s: String): IO[A] =
    IO.fromEither(io.circe.parser.parse(s).flatMap(_.as[A]))

  def serialise[A: Encoder](v: A): IO[String] =
    import io.circe.syntax.*
    IO.pure(v.asJson.noSpacesSortKeys)

  test("Identifier serialisation roundtrip") {
    forall { (l: Long) =>
      val id = Identifier.from(l)
      serialise(id).flatMap(parse[Identifier]).map { result =>
        expect(result == id)
      }
    }
  }

  opaque type MyId = Identifier
  object MyId extends OpaqueId[MyId]
  
  test("OpaqueId serialisation roundtrip") {
    forall { (l: Long) =>
      val id = MyId.fromLong(l)
      serialise(id).flatMap(parse[MyId]).map { result =>
        expect(result == id)
      }
    }
  }

  opaque type MyNumber = Int
  object MyNumber extends OpaqueIntegral[MyNumber, Int]

  test("OpaqueIntegral serialisation test: Int") {

    forall { (l: Int) =>
      val id = MyNumber.from(l)
      serialise(id).flatMap(parse[MyNumber]).map { result =>
        expect(result == id)
      }
    }
  }
end JsonCodecsTest
