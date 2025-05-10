package smithy4s.json.circe

import smithy4s.Schema
import io.circe.Codec
import smithy4s.json.circe.internals._

object Json {

  private val circe = new CirceCodecCompilerImpl(1024, None)

  private val circeCodecGlobalCache = circe.createCache()

  implicit def deriveJsonCodec[A: Schema]: Codec[A] =
    circe.fromSchema(implicitly[Schema[A]], circeCodecGlobalCache)
}
