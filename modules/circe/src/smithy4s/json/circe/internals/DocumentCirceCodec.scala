package smithy4s.json.circe.internals

import smithy4s.Document
import smithy4s.Document.{Encoder => _, Decoder => _, _}
import io.circe._

private[smithy4s] object DocumentCirceCodec {

  val documentToJson: Document => Json = {
    case DNull           => Json.Null
    case DString(value)  => Json.fromString(value)
    case DBoolean(value) => Json.fromBoolean(value)
    case DNumber(value)  => Json.fromBigDecimal(value)
    case DArray(values)  => Json.fromValues(values.map(documentToJson))
    case DObject(entries) =>
      Json.fromFields(entries.view.mapValues(documentToJson))
  }

  def fromJson(json: Json): Document = json.fold(
    jsonNull = DNull,
    jsonBoolean = DBoolean(_),
    jsonNumber = n => DNumber(n.toBigDecimal.get),
    jsonString = DString(_),
    jsonArray = arr => DArray(arr.map(fromJson)),
    jsonObject = obj => DObject(obj.toMap.view.mapValues(fromJson).toMap)
  )

  implicit val circeCodecForDocument: Codec[Document] = Codec.from(
    Decoder.instance(_.as[Json].map(fromJson)),
    Encoder.instance(documentToJson)
  )
}
