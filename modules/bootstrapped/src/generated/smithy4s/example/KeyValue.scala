package smithy4s.example

import smithy4s.Hints
import smithy4s.Schema
import smithy4s.ShapeId
import smithy4s.ShapeTag
import smithy4s.schema.Schema.string
import smithy4s.schema.Schema.struct

final case class KeyValue(key: String, value: String)

object KeyValue extends ShapeTag.Companion[KeyValue] {
  val id: ShapeId = ShapeId("smithy4s.example", "KeyValue")

  val hints: Hints = Hints.empty

  // constructor using the original order from the spec
  private def make(key: String, value: String): KeyValue = KeyValue(key, value)

  implicit val schema: Schema[KeyValue] = struct(
    string.required[KeyValue]("key", _.key),
    string.required[KeyValue]("value", _.value),
  ){
    make
  }.withId(id).addHints(hints)
}
