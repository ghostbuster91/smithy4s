package smithy4s.example

import smithy4s.Hints
import smithy4s.Schema
import smithy4s.ShapeId
import smithy4s.ShapeTag
import smithy4s.schema.Schema.struct

final case class GetEnumInput(aa: TheEnum)

object GetEnumInput extends ShapeTag.Companion[GetEnumInput] {
  val id: ShapeId = ShapeId("smithy4s.example", "GetEnumInput")

  val hints: Hints = Hints.empty

  // constructor using the original order from the spec
  private def make(aa: TheEnum): GetEnumInput = GetEnumInput(aa)

  implicit val schema: Schema[GetEnumInput] = struct(
    TheEnum.schema.required[GetEnumInput]("aa", _.aa).addHints(smithy.api.HttpLabel()),
  ){
    make
  }.withId(id).addHints(hints)
}
