package smithy4s.json.circe.internals

import io.circe._
import smithy4s.schema._
import smithy4s.HintMask

private[smithy4s] case class CirceCodecCompilerImpl(
    maxArity: Int,
    hintMask: Option[HintMask]
) extends CachedSchemaCompiler.Impl[Codec] {

  type Aux[A] = Codec[A]

  def fromSchema[A](schema: Schema[A], cache: Cache): Codec[A] = {
    val visitor = new CirceSchemaVisitor(
      maxArity,
      cache
    )
    val amendedSchema =
      hintMask
        .map(mask => schema.transformHintsTransitively(mask.apply))
        .getOrElse(schema)
    amendedSchema.compile(visitor)
  }

}
