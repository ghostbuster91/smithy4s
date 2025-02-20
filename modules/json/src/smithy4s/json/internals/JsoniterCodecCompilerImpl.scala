/*
 *  Copyright 2021-2025 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s.json
package internals

import smithy4s.HintMask
import smithy4s.schema._
import smithy4s.codecs.FieldRenderPredicateCompiler

private[smithy4s] case class JsoniterCodecCompilerImpl(
    maxArity: Int,
    flexibleCollectionsSupport: Boolean,
    infinitySupport: Boolean,
    preserveMapOrder: Boolean,
    hintMask: Option[HintMask],
    lenientTaggedUnionDecoding: Boolean,
    lenientNumericDecoding: Boolean,
    fieldRenderPredicateCompiler: FieldRenderPredicateCompiler
) extends CachedSchemaCompiler.Impl[JCodec]
    with JsoniterCodecCompiler {

  type Aux[A] = JCodec[A]

  def withMaxArity(max: Int): JsoniterCodecCompiler = copy(maxArity = max)

  @deprecated(
    message = "Use `withFieldDefaultEncodingCompiler` instead",
    since = "0.18.25"
  )
  def withExplicitDefaultsEncoding(
      explicitNulls: Boolean
  ): JsoniterCodecCompiler =
    withFieldRenderPredicateCompiler(
      FieldRenderPredicateCompiler.fromExplicitDefaults(explicitNulls)
    )

  def withFieldRenderPredicateCompiler(
      fieldRenderPredicateCompiler: FieldRenderPredicateCompiler
  ): JsoniterCodecCompiler =
    copy(fieldRenderPredicateCompiler = fieldRenderPredicateCompiler)

  def withHintMask(hintMask: HintMask): JsoniterCodecCompiler =
    copy(hintMask = Some(hintMask))

  def withFlexibleCollectionsSupport(
      flexibleCollectionsSupport: Boolean
  ): JsoniterCodecCompiler =
    copy(flexibleCollectionsSupport = flexibleCollectionsSupport)

  def withInfinitySupport(infinitySupport: Boolean): JsoniterCodecCompiler =
    copy(infinitySupport = infinitySupport)

  def withMapOrderPreservation(
      preserveMapOrder: Boolean
  ): JsoniterCodecCompiler =
    copy(preserveMapOrder = preserveMapOrder)

  def withLenientTaggedUnionDecoding: JsoniterCodecCompiler =
    copy(lenientTaggedUnionDecoding = true)

  def withLenientNumericDecoding: JsoniterCodecCompiler =
    copy(lenientNumericDecoding = true)

  def fromSchema[A](schema: Schema[A], cache: Cache): JCodec[A] = {
    val visitor = new SchemaVisitorJCodec(
      maxArity,
      infinitySupport,
      flexibleCollectionsSupport,
      preserveMapOrder,
      lenientTaggedUnionDecoding,
      lenientNumericDecoding,
      cache,
      fieldRenderPredicateCompiler
    )
    val amendedSchema =
      hintMask
        .map(mask => schema.transformHintsTransitively(mask.apply))
        .getOrElse(schema)
    amendedSchema.compile(visitor)
  }

}

private[smithy4s] object JsoniterCodecCompilerImpl {

  val defaultJsoniterCodecCompiler: JsoniterCodecCompiler =
    JsoniterCodecCompilerImpl(
      maxArity = JsoniterCodecCompiler.defaultMaxArity,
      fieldRenderPredicateCompiler =
        FieldRenderPredicateCompiler.SkipIfEmptyOrDefaultOptionals,
      infinitySupport = false,
      flexibleCollectionsSupport = false,
      preserveMapOrder = false,
      lenientTaggedUnionDecoding = false,
      lenientNumericDecoding = false,
      hintMask = Some(JsoniterCodecCompiler.defaultHintMask)
    )

}
