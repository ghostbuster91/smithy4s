package smithy4s.json.circe.internals

import smithy.api.JsonName
import alloy.Untagged
import io.circe._
import smithy4s.schema._
import smithy4s.schema.Primitive._
import smithy4s.{Blob, Timestamp}
import java.util.Base64
import smithy.api.TimestampFormat
import cats.syntax.all._
import smithy4s.{Bijection, Hints, Lazy, Refinement, ShapeId}

import smithy4s.capability.EncoderK

private[smithy4s] class CirceSchemaVisitor(
    maxArity: Int,
    val cache: CompilationCache[Codec]
) extends SchemaVisitor.Cached[Codec] { self =>

  override def primitive[P](
      shapeId: ShapeId,
      hints: Hints,
      tag: Primitive[P]
  ): Codec[P] =
    tag match {
      case PString  => Codec.from(Decoder.decodeString, Encoder.encodeString)
      case PBoolean => Codec.from(Decoder.decodeBoolean, Encoder.encodeBoolean)
      case PInt     => Codec.from(Decoder.decodeInt, Encoder.encodeInt)
      case PLong    => Codec.from(Decoder.decodeLong, Encoder.encodeLong)
      case PShort   => Codec.from(Decoder.decodeShort, Encoder.encodeShort)
      case PByte    => Codec.from(Decoder.decodeByte, Encoder.encodeByte)
      case PDouble  => Codec.from(Decoder.decodeDouble, Encoder.encodeDouble)
      case PFloat   => Codec.from(Decoder.decodeFloat, Encoder.encodeFloat)
      case PBigInt  => Codec.from(Decoder.decodeBigInt, Encoder.encodeBigInt)
      case PBigDecimal =>
        Codec.from(Decoder.decodeBigDecimal, Encoder.encodeBigDecimal)
      case PUUID     => Codec.from(Decoder.decodeUUID, Encoder.encodeUUID)
      case PDocument => DocumentCirceCodec.circeCodecForDocument
      case PBlob =>
        Codec.from(
          Decoder.decodeString.map(Base64.getDecoder.decode).map(Blob(_)),
          Encoder.encodeString
            .contramap(Base64.getEncoder.encodeToString)
            .contramap(_.toArray)
        )
      case PTimestamp =>
        Codec.from(
          Decoder.decodeString.emap(s =>
            Timestamp
              .parse(s, TimestampFormat.DATE_TIME)
              .toRight("Invalid timestamp")
          ),
          Encoder.encodeString.contramap(_.format(TimestampFormat.DATE_TIME))
        )
    }

  override def collection[C[_], A](
      shapeId: ShapeId,
      hints: Hints,
      tag: CollectionTag[C],
      member: Schema[A]
  ): Codec[C[A]] =
    tag match {
      case CollectionTag.ListTag =>
        CollectionCirceCodecs.listCodec(member.compile(this), maxArity)
      case CollectionTag.VectorTag =>
        CollectionCirceCodecs.vectorCodec(member.compile(this), maxArity)
      case CollectionTag.SetTag =>
        CollectionCirceCodecs.setCodec(member.compile(this), maxArity)
      case CollectionTag.IndexedSeqTag =>
        CollectionCirceCodecs.indexedSeqCodec(member.compile(this), maxArity)
    }

  override def option[A](schema: Schema[A]): Codec[Option[A]] = {
    val inner = schema.compile(this)
    Codec.from(
      Decoder.decodeOption(inner),
      Encoder.encodeOption(inner)
    )
  }

  override def struct[S](
      shapeId: ShapeId,
      hints: Hints,
      fields: Vector[Field[S, _]],
      make: IndexedSeq[Any] => S
  ): Codec[S] = {
    val decoders = fields.zipWithIndex.map { case (field, idx) =>
      val jsonName =
        field.hints.get(JsonName).map(_.value).getOrElse(field.label)
      val decoder = field.schema.compile(this)
      (jsonName, decoder.map(a => (idx, a)))
    }.toMap

    val decoder: Decoder[S] = Decoder.instance { c =>
      val obj = c.value.asObject.getOrElse(JsonObject.empty)
      val decodeObjResults =
        decoders.foldLeft(IndexedSeq.empty[Any].asRight[DecodingFailure]) {
          case (Right(seq), (key, decoderWithIdx)) =>
            val jsonField = obj(key).get
            decoderWithIdx
              .decodeJson(jsonField)
              .map(r => seq :+ r)
          case (err @ Left(_), _) => err
        }
      decodeObjResults.map(make)
    }

    val encoder: Encoder[S] = Encoder.instance { s =>
      def encodeField[A](f: Field[S, A]) = {
        val encoder = apply(f.schema)
        encoder(f.get(s))
      }
      val fieldsJson = fields.map { field =>
        val jsonName =
          field.hints.get(JsonName).map(_.value).getOrElse(field.label)
        jsonName -> encodeField(field)
      }
      Json.obj(fieldsJson: _*)
    }

    Codec.from(decoder, encoder)
  }

  override def union[U](
      shapeId: ShapeId,
      hints: Hints,
      alts: Vector[Alt[U, _]],
      dispatch: Alt.Dispatcher[U]
  ): Codec[U] =
    hints match {
      case Untagged.hint(_) =>
        untaggedUnionCodec(alts)(dispatch)
      case _ if hints.get(JsonName).exists(_.value == "lenient") =>
        taggedUnionCodec(alts, lenient = true)(dispatch)
      case _ =>
        taggedUnionCodec(alts)(dispatch)
    }

  override def enumeration[E](
      shapeId: ShapeId,
      hints: Hints,
      tag: EnumTag[E],
      values: List[EnumValue[E]],
      total: E => EnumValue[E]
  ): Codec[E] = tag match {
    case EnumTag.ClosedStringEnum =>
      val mapping = values.map(ev => ev.stringValue -> ev.value).toMap
      val decodeStr: String => Either[String, E] =
        str => mapping.get(str).toRight(s"Invalid enum value: $str")

      Codec.from(
        Decoder.decodeString.emap(decodeStr),
        Encoder.encodeString.contramap(e => total(e).stringValue)
      )

    case EnumTag.OpenStringEnum(unknown) =>
      val mapping = values.map(ev => ev.stringValue -> ev.value).toMap
      Codec.from(
        Decoder.decodeString.map(str => mapping.getOrElse(str, unknown(str))),
        Encoder.encodeString.contramap(e => total(e).stringValue)
      )

    case EnumTag.ClosedIntEnum =>
      val mapping = values.map(ev => ev.intValue -> ev.value).toMap
      val decodeInt: Int => Either[String, E] =
        i => mapping.get(i).toRight(s"Invalid enum int value: $i")

      Codec.from(
        Decoder.decodeInt.emap(decodeInt),
        Encoder.encodeInt.contramap(e => total(e).intValue)
      )

    case EnumTag.OpenIntEnum(unknown) =>
      val mapping = values.map(ev => ev.intValue -> ev.value).toMap
      Codec.from(
        Decoder.decodeInt.map(i => mapping.getOrElse(i, unknown(i))),
        Encoder.encodeInt.contramap(e => total(e).intValue)
      )
  }

  override def map[K, V](
      shapeId: ShapeId,
      hints: Hints,
      key: Schema[K],
      value: Schema[V]
  ): Codec[Map[K, V]] = {
    val keyCodec = key.compile(this)
    val valueCodec = value.compile(this)

    key match {
      case Schema.PrimitiveSchema(_, _, Primitive.PString) =>
        // K is a String → standard JSON object
        implicit val kDecoder: KeyDecoder[K] = (keyStr: String) =>
          keyCodec.decodeJson(Json.fromString(keyStr)).toOption
        implicit val kEncoder: KeyEncoder[K] = (k: K) =>
          keyCodec(k).asString.getOrElse(k.toString)

        Codec.from(
          Decoder.decodeMap[K, V](kDecoder, valueCodec),
          Encoder.encodeMap[K, V](kEncoder, valueCodec)
        )
      case _ =>
        // fallback to array of {"key": ..., "value": ...}
        val kvSchema = Schema.struct[(K, V)](
          Field.required("key", key, _._1),
          Field.required("value", value, _._2)
        )((k, v) => (k, v))

        val kvCodec = kvSchema.compile(this)

        Codec.from(
          Decoder.decodeList(kvCodec).map(_.toMap),
          Encoder.encodeList(kvCodec).contramap(_.toList)
        )
    }
  }

  override def biject[A, B](
      schema: Schema[A],
      bijection: Bijection[A, B]
  ): Codec[B] = {
    val base: Codec[A] = schema.compile(this)
    base.imap(bijection.apply)(bijection.from)
  }

  override def refine[A, B](
      schema: Schema[A],
      refinement: Refinement[A, B]
  ): Codec[B] = {
    val base: Codec[A] = schema.compile(this)

    Codec.from(
      base.emap(a => refinement(a)),
      base.contramap(refinement.from)
    )
  }

  override def lazily[A](suspend: Lazy[Schema[A]]): Codec[A] = {
    lazy val underlying: Codec[A] = suspend.value.compile(this)
    Codec.from(
      Decoder.instance(c => underlying.tryDecode(c)),
      Encoder.instance(a => underlying(a))
    )
  }

  private implicit val encoderKEncoder: EncoderK[Encoder, Json] =
    new EncoderK[Encoder, Json] {
      override def apply[A](fa: Encoder[A], a: A): Json = fa(a)

      override def absorb[A](f: A => Json): Encoder[A] = Encoder(a => f(a))
    }

  private def processAlt[U, A](alt: Alt[U, A]) =
    alt.schema.compile(this).map(alt.inject)

  def taggedUnionCodec[U](
      alternatives: Vector[Alt[U, _]],
      lenient: Boolean = false,
      jsonUnknownHandler: Option[(String, Json) => Decoder.Result[U]] = None
  )(dispatch: Alt.Dispatcher[U]): Codec[U] = {

    val altDecoders: Map[String, Decoder[U]] =
      alternatives.map { alt =>
        val label = alt.hints.get(JsonName).map(_.value).getOrElse(alt.label)
        val decoder = processAlt(alt)
        label -> decoder
      }.toMap

    val encoder: Encoder[U] = dispatch.compile(new Alt.Precompiler[Encoder] {
      def apply[A](label: String, schema: Schema[A]): Encoder[A] = {
        val encoderA = schema.compile(self)
        encoderA
      }
    })

    if (lenient)
      LenientTaggedUnionCodec(altDecoders, encoder)
    else
      TaggedUnionCodec(altDecoders, encoder, jsonUnknownHandler)
  }

  def untaggedUnionCodec[U](
      alternatives: Vector[Alt[U, _]]
  )(dispatch: Alt.Dispatcher[U]): Codec[U] = {
    val decoders = alternatives.map(alt => processAlt(alt))
    val encoder: Encoder[U] = dispatch.compile(new Alt.Precompiler[Encoder] {
      def apply[A](label: String, schema: Schema[A]): Encoder[A] =
        schema.compile(self)
    })

    UntaggedUnionCodec(decoders.toList, encoder)
  }

  private case class UntaggedUnionCodec[U](
      alternatives: List[Decoder[U]],
      encoder: Encoder[U]
  ) extends Codec[U] {

    override def apply(c: HCursor): Decoder.Result[U] = {
      val errors = scala.collection.mutable.ListBuffer.empty[DecodingFailure]

      alternatives.iterator
        .map(_.tryDecode(c))
        .collectFirst {
          case Right(result) => Right(result)
          case Left(err) =>
            errors += err
            null
        }
        .find(_ != null)
        .getOrElse {
          Left(DecodingFailure("None of the union decoders matched", c.history))
        }
    }

    override def apply(a: U): Json = encoder(a)
  }

  private case class LenientTaggedUnionCodec[U](
      alternatives: Map[String, Decoder[U]],
      encoder: Encoder[U]
  ) extends Codec[U] {

    override def apply(c: HCursor): Decoder.Result[U] =
      c.keys.toList
        .flatMap(_.toList)
        .collectFirstSome { key =>
          c.downField(key)
            .success
            .flatMap(_.focus)
            .filterNot(_.isNull)
            .flatMap { json =>
              alternatives.get(key).map(_.decodeJson(json))
            }
        }
        .getOrElse(
          Left(
            DecodingFailure(
              "Expected a single non-null tagged field",
              c.history
            )
          )
        )

    override def apply(a: U): Json = encoder(a)
  }

  private case class TaggedUnionCodec[U](
      alternatives: Map[String, Decoder[U]],
      encoder: Encoder[U],
      unknownLabelHandler: Option[(String, Json) => Decoder.Result[U]] = None
  ) extends Codec[U] {

    override def apply(c: HCursor): Decoder.Result[U] =
      for {
        obj <- c.as[JsonObject]
        (label, value) <- obj.toMap.headOption.toRight(
          DecodingFailure("Expected a single key/value pair", c.history)
        )
        decoded <- alternatives.get(label) match {
          case Some(dec) => dec.tryDecode(value.hcursor)
          case None =>
            unknownLabelHandler
              .map(handler => handler(label, value))
              .getOrElse(
                Left(DecodingFailure(s"Unknown tag: $label", c.history))
              )
        }
      } yield decoded

    override def apply(a: U): Json = encoder(a)
  }

}
