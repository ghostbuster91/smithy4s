package smithy4s.json.circe.internals

import io.circe._

private[smithy4s] object CollectionCirceCodecs {

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

  def listCodec[A](a: Codec[A], maxArity: Int): Codec[List[A]] =
    withMaxArity[A, List](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      identity,
      identity,
      maxArity,
      "List"
    )

  def vectorCodec[A](a: Codec[A], maxArity: Int): Codec[Vector[A]] =
    withMaxArity[A, Vector](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      _.toVector,
      _.toList,
      maxArity,
      "Vector"
    )

  def indexedSeqCodec[A](
      a: Codec[A],
      maxArity: Int
  ): Codec[IndexedSeq[A]] =
    withMaxArity[A, IndexedSeq](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      _.toIndexedSeq,
      _.toList,
      maxArity,
      "IndexedSeq"
    )

  def setCodec[A](a: Codec[A], maxArity: Int): Codec[Set[A]] =
    withMaxArity[A, Set](
      Decoder.decodeList(a),
      Encoder.encodeList(a),
      _.toSet,
      _.toList,
      maxArity,
      "Set"
    )
}
