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

package smithy4s
package dynamic

/**
  * A dynamically loaded schema index, containing existential instances
  * of services and schemas, that can be used to wire protocols together
  * without requiring code generation.
  */
trait DynamicSchemaIndex extends DynamicSchemaIndexPlatform {
  def allServices: Iterable[DynamicSchemaIndex.ServiceWrapper]
  def getService(shapeId: ShapeId): Option[DynamicSchemaIndex.ServiceWrapper] =
    allServices.find(_.service.id == shapeId)

  def allSchemas: Iterable[Schema[_]]
  def getSchema(shapeId: ShapeId): Option[Schema[_]]

  def metadata: Map[String, Document]
}

object DynamicSchemaIndex extends DynamicSchemaIndexCompanionPlatform {

  /**
    * Loads the model from a dynamic representation of smithy models
    * (typically json blobs). This representation is modelled in smithy itself,
    * and code generated by smithy4s.
    *
    * @param model
    */
  def load(
      model: dynamic.model.Model
  ): DynamicSchemaIndex =
    internals.Compiler.compile(model)

  /**
    * A construct that hides the types a service instance works,
    * virtually turning them into existential types.
    *
    * This prevents the user from calling the algebra/transformation
    * in an unsafe fashion.
    */
  trait ServiceWrapper {
    type Alg[P[_, _, _, _, _]]

    def service: Service[Alg]
  }

  /**
    * Returns a builder for creating a DynamicSchemaIndex manually
    */
  def builder: Builder = new BuilderImpl(List.empty, Map.empty)

  // scalafmt: {maxColumn = 120}
  trait Builder {

    def addService[Alg[_[_, _, _, _, _]]](implicit service: Service[Alg]): Builder
    def addSchema[A](implicit schema: Schema[A]): Builder

    final def addAll(convertibles: SmithyConvertible*): Builder =
      convertibles.foldLeft(this) {
        case (builder, fs: SmithyConvertible.FromService[alg]) => builder.addService(fs.service)
        case (builder, fs: SmithyConvertible.FromSchema[a])    => builder.addSchema(fs.schema)
      }

    def build(): DynamicSchemaIndex

  }

  sealed trait SmithyConvertible
  object SmithyConvertible {

    implicit def serviceToSmithyConvertible[Alg[_[_, _, _, _, _]]](service: Service[Alg]): SmithyConvertible =
      FromService(service)

    implicit def schemaToSmithyConvertible[A](schema: Schema[A]): SmithyConvertible =
      FromSchema(schema)

    protected[DynamicSchemaIndex] final case class FromService[Alg[_[_, _, _, _, _]]](service: Service[Alg])
        extends SmithyConvertible
    protected[DynamicSchemaIndex] final case class FromSchema[A](schema: Schema[A]) extends SmithyConvertible
  }
  private final case class WrappedService[Alg0[_[_, _, _, _, _]]](
      val svc: Service[Alg0]
  ) extends DynamicSchemaIndex.ServiceWrapper {
    type Alg[P[_, _, _, _, _]] = Alg0[P]
    def service: Service[Alg] = svc
  }

  /**
  * Builder for creating a `DynamicSchemaIndex` manually
  */
  private final case class BuilderImpl(
      services: List[DynamicSchemaIndex.ServiceWrapper],
      schemas: Map[ShapeId, Schema[_]]
  ) extends Builder {

    /**
    * Add a `smithy4s.Service` which will be included when building the `DynamicSchemaIndex`
    */
    def addService[Alg0[_[_, _, _, _, _]]](implicit service: Service[Alg0]): Builder =
      this.copy(services = this.services :+ WrappedService(service))

    /**
    * Add a `smithy4s.Schema`s which will be included when building the `DynamicSchemaIndex`
    */
    def addSchema[A](implicit schema: Schema[A]): Builder =
      this.copy(schemas = (this.schemas + (schema.shapeId -> schema)))

    /**
    * Build the DynamicSchemaIndex using the information in this builder.
    *
    * Note that the functions in this `DynamicSchemaIndex`, including `allServices`,
    * `allSchemas`, and `getSchema` will only return the items that were placed into
    * the builder directly and NOT the transitive values. Additionally, the metadata
    * is always empty.
    */
    def build(): DynamicSchemaIndex =
      new DynamicSchemaIndex {
        def allServices: Iterable[DynamicSchemaIndex.ServiceWrapper] =
          services

        def allSchemas: Iterable[Schema[_]] = schemas.values

        def getSchema(shapeId: ShapeId): Option[Schema[_]] =
          schemas.get(shapeId)

        def metadata: Map[String, Document] = Map.empty
      }
  }

}
