/*
 *  Copyright 2021-2024 Disney Streaming
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

import smithy4s.schema._
import schema.ErrorSchema
import smithy4s.kinds.PolyFunction5
import smithy4s.kinds._

/**
  * A representation of a smithy operation.
  *
  * @tparam Op: the GADT of all operations in a service
  * @tparam I: the input type of the operation (Unit if N/A)
  * @tparam E: the error ADT of the operation (Nothing if N/A)
  * @tparam O: the output of the operation (Unit if N/A)
  * @tparam SI: the Streamed input of the operaton (Nothing if N/A)
  * @tparam SO: the Streamed output of the operaton (Nothing if N/A)
  *
  * This type carries references to the Schemas of the various types involved,
  * allowing to compile corresponding codecs.
  *
  * Optionally, an endpoint can have an `ErrorSchema` which allows for matching
  * throwables against the errors the operation knows about (which form an ADT
  * in the Scala representation)
  *
  * NB: SI an SO respectively are derived from the @streaming trait in smithy.
  * If this trait is present in one on one of the members of Input/Output, the
  * member is removed from the Scala representation, in order to avoid polluting
  * datatypes that typically fit in memory with concerns of streaming (which can
  * be encoded a great many ways, using a great many libraries)
  */
// scalafmt: {maxColumn = 120}
trait Endpoint[Op[_, _, _, _, _], I, E, O, SI, SO] { self =>

  final def mapSchema(
      f: OperationSchema[I, E, O, SI, SO] => OperationSchema[I, E, O, SI, SO]
  ): Endpoint[Op, I, E, O, SI, SO] = Endpoint(f(schema), wrap)

  def schema: OperationSchema[I, E, O, SI, SO]
  def wrap(input: I): Op[I, E, O, SI, SO]

  final def id: ShapeId = schema.id
  final def name: String = schema.id.name
  final def hints: Hints = schema.hints
  final def input: Schema[I] = schema.input
  final def output: Schema[O] = schema.output
  final def error: Option[ErrorSchema[E]] = schema.error
  @deprecated("Use .error instead", since = "0.18")
  final def errorschema: Option[ErrorSchema[E]] = schema.error
  final def streamedInput: Option[StreamingSchema[SI]] = schema.streamedInput
  final def streamedOutput: Option[StreamingSchema[SO]] = schema.streamedOutput

  object Error {
    def unapply(throwable: Throwable): Option[(ErrorSchema[E], E)] =
      error.flatMap { err =>
        err.liftError(throwable).map(err -> _)
      }
  }

  /**
    * Allows the creation of a handler via lifting a function that returns some functor.
    */
  def handler[F[_]](f: I => F[O]): EndpointHandler[Op, Kind1[F]#toKind5] = new Handler[F] {
    def run(input: I): F[O] = f(input)
  }

  /**
    * Allows the creation of a handler via lifting a function that returns some bi-functor.
    */
  def errorAwareHandler[F[_, _]](f: I => F[E, O]): EndpointHandler[Op, Kind2[F]#toKind5] = new ErrorAwareHandler[F] {
    def run(input: I): F[E, O] = f(input)
  }

  /**
    * Allows the creation of a hander via object-oriented inheritance.
    */
  abstract class Handler[F[_]] extends EndpointHandler[Op, Kind1[F]#toKind5] {
    def run(input: I): F[O]
    protected[smithy4s] def lift[Alg[_[_, _, _, _, _]]](
        service: Service.Aux[Alg, Op]
    ): PolyFunction5[Op, Kind1[F]#optional5] = new PolyFunction5[Op, Kind1[F]#optional5] {
      val ord = service.endpoints.indexOf(self)

      def apply[I_, E_, O_, SI_, SO_](op: Op[I_, E_, O_, SI_, SO_]): Option[F[O_]] = if (service.ordinal(op) == ord) {
        Some(run(service.input(op).asInstanceOf[I]).asInstanceOf[F[O_]])
      } else None
    }
  }

  abstract class ErrorAwareHandler[F[_, _]] extends EndpointHandler[Op, Kind2[F]#toKind5] {
    def run(input: I): F[E, O]
    protected[smithy4s] def lift[Alg[_[_, _, _, _, _]]](
        service: Service.Aux[Alg, Op]
    ): PolyFunction5[Op, Kind2[F]#optional5] = new PolyFunction5[Op, Kind2[F]#optional5] {
      val ord = service.endpoints.indexOf(self)

      def apply[I_, E_, O_, SI_, SO_](op: Op[I_, E_, O_, SI_, SO_]): Option[F[E_, O_]] = if (
        service.ordinal(op) == ord
      ) {
        Some(run(service.input(op).asInstanceOf[I]).asInstanceOf[F[E_, O_]])
      } else None
    }
  }
}

object Endpoint {

  trait Middleware[A] { self =>
    def prepare[Alg[_[_, _, _, _, _]]](service: Service[Alg])(endpoint: service.Endpoint[_, _, _, _, _]): A => A
    final def biject[B](to: A => B)(from: B => A): Middleware[B] = new Middleware[B] {
      def prepare[Alg[_[_, _, _, _, _]]](service: Service[Alg])(endpoint: service.Endpoint[_, _, _, _, _]): B => B =
        self.prepare(service)(endpoint).compose(from).andThen(to)
    }

    final def andThen(other: Middleware[A]): Middleware[A] =
      if (this == Middleware.NoopMiddleware) { other }
      else if (other == Middleware.NoopMiddleware) { this }
      else
        new Middleware[A] {
          def prepare[Alg[_[_, _, _, _, _]]](
              service: Service[Alg]
          )(endpoint: service.Endpoint[_, _, _, _, _]): A => A =
            self
              .prepare(service)(endpoint)
              .andThen(other.prepare(service)(endpoint))
        }

  }
// format: on

  object Middleware {

    trait Simple[Construct] extends Middleware[Construct] {
      def prepareWithHints(serviceHints: Hints, endpointHints: Hints): Construct => Construct

      final def prepare[Alg[_[_, _, _, _, _]]](service: Service[Alg])(
          endpoint: service.Endpoint[_, _, _, _, _]
      ): Construct => Construct =
        prepareWithHints(service.hints, endpoint.hints)
    }

    trait Standard[Construct] extends Middleware[Construct] {
      def prepare(
          serviceId: ShapeId,
          endpointId: ShapeId,
          serviceHints: Hints,
          endpointHints: Hints
      ): Construct => Construct

      final def prepare[Alg[_[_, _, _, _, _]]](service: Service[Alg])(
          endpoint: service.Endpoint[_, _, _, _, _]
      ): Construct => Construct =
        prepare(service.id, endpoint.id, service.hints, endpoint.hints)
    }

    def noop[Construct]: Middleware[Construct] =
      NoopMiddleware.asInstanceOf[Middleware[Construct]]

    private case object NoopMiddleware extends Middleware[Any] {
      def prepare[Alg[_[_, _, _, _, _]]](service: Service[Alg])(
          endpoint: service.Endpoint[_, _, _, _, _]
      ): Any => Any = identity[Any]
    }

  }
  def apply[Op[_, _, _, _, _], I, E, O, SI, SO](
      operationSchema: OperationSchema[I, E, O, SI, SO],
      wrapFunction: I => Op[I, E, O, SI, SO]
  ): Endpoint[Op, I, E, O, SI, SO] =
    new Endpoint[Op, I, E, O, SI, SO] {
      def schema = operationSchema
      def wrap(i: I): Op[I, E, O, SI, SO] = wrapFunction(i)
    }

  type ForOperation[Op[_, _, _, _, _]] = {
    type e[I, E, O, SI, SO] = Endpoint[Op, I, E, O, SI, SO]
  }

}
