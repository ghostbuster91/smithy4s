package smithy4s.json.circe.internals

import io.circe._

object CollectionCirceCodecs {

  def withMaxArity[A, C[_]](
      decoder: Decoder[List[A]],
      encoder: Encoder[List[A]],
      toC: List[A] => C[A],
      fromC: C[A] => List[A],
      maxArity: Int,
      typeName: String
  ): Codec[C[A]] = Codec.from(
    decoder.emap { list =>
      if (list.size > maxArity)
        Left(s"$typeName exceeded max arity of $maxArity (got ${list.size})")
      else Right(toC(list))
    },
    Encoder.instance(ca => encoder(fromC(ca)))
  )

  def listCodec[A](implicit a: Codec[A], maxArity: Int = 256): Codec[List[A]] =
    withMaxArity[A, List](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      identity,
      identity,
      maxArity,
      "List"
    )

  def vectorCodec[A](implicit
      a: Codec[A],
      maxArity: Int = 256
  ): Codec[Vector[A]] =
    withMaxArity[A, Vector](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      _.toVector,
      _.toList,
      maxArity,
      "Vector"
    )

  def indexedSeqCodec[A](implicit
      a: Codec[A],
      maxArity: Int = 256
  ): Codec[IndexedSeq[A]] =
    withMaxArity[A, IndexedSeq](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      _.toIndexedSeq,
      _.toList,
      maxArity,
      "IndexedSeq"
    )

  def setCodec[A](implicit a: Codec[A], maxArity: Int = 256): Codec[Set[A]] =
    withMaxArity[A, Set](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      _.toSet,
      _.toList,
      maxArity,
      "Set"
    )
}
