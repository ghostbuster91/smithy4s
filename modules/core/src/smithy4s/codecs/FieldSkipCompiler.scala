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

package smithy4s.codecs

import smithy4s.schema.Field
import smithy4s.schema.Schema

trait FieldSkipCompiler { self =>
  def compile[A](
      field: Field[?, A]
  ): FieldSkipCompiler.ShouldSkip[A]

  def combine(
      other: FieldSkipCompiler
  ): FieldSkipCompiler =
    new FieldSkipCompiler {

      def compile[A](
          field: Field[?, A]
      ): FieldSkipCompiler.ShouldSkip[A] = {
        val r1 = self.compile(field)
        val r2 = other.compile(field)
        a => r1(a) || r2(a)
      }
    }
}

object FieldSkipCompiler {

  type ShouldSkip[A] = A => Boolean

  private trait SkipNonRequired extends FieldSkipCompiler {

    final def compile[A](
        field: Field[?, A]
    ): FieldSkipCompiler.ShouldSkip[A] = {
      if (field.isRequired) Function.const(false)
      else compileOptional(field)

    }

    def compileOptional[A](
        field: Field[?, A]
    ): FieldSkipCompiler.ShouldSkip[A]

  }

  val NeverSkip: FieldSkipCompiler =
    new FieldSkipCompiler {
      def compile[A](field: Field[_, A]): ShouldSkip[A] = Function.const(false)
    }

  private def asEmptyCollectionPredicate[F[_], A](
      schema: Schema[A]
  ): Option[A => Boolean] = {
    import Schema._
    schema match {
      case c: CollectionSchema[f, a] =>
        Some(collection => c.tag.isEmpty(collection.asInstanceOf[f[a]]))
      case BijectionSchema(inner, _)  => asEmptyCollectionPredicate(inner)
      case RefinementSchema(inner, _) => asEmptyCollectionPredicate(inner)
      case OptionSchema(inner) =>
        asEmptyCollectionPredicate(inner)
          .map(predicate =>
            wrappedCollection =>
              wrappedCollection.asInstanceOf[Option[Any]].exists(predicate)
          )
      case LazySchema(_)           => None // ?
      case _: MapSchema[k, v]      => None
      case _: EnumerationSchema[_] => None
      case _: StructSchema[_]      => None
      case _: UnionSchema[_]       => None
      case _: PrimitiveSchema[_]   => None
    }
  }

  val SkipIfEmptyOptionalCollection: FieldSkipCompiler =
    new FieldSkipCompiler.SkipNonRequired {

      def compileOptional[A](field: Field[?, A]): ShouldSkip[A] = {
        asEmptyCollectionPredicate(field.schema) match {
          case None          => Function.const(false)
          case Some(isEmpty) => isEmpty
        }
      }
    }

  val SkipIfEmptyCollection: FieldSkipCompiler =
    new FieldSkipCompiler {

      def compile[A](field: Field[_, A]): ShouldSkip[A] = {
        asEmptyCollectionPredicate(field.schema) match {
          case None          => Function.const(false)
          case Some(isEmpty) => isEmpty
        }
      }
    }

  val SkipIfDefaultOptionals: FieldSkipCompiler =
    new FieldSkipCompiler.SkipNonRequired {
      def compileOptional[A](field: Field[?, A]): ShouldSkip[A] = {
        // Optional fields have None as their default, so we need to make sure not to skip them here
        a => a != None && field.isDefaultValue(a)
      }
    }

  val SkipIfEmptyOptionals: FieldSkipCompiler =
    new FieldSkipCompiler.SkipNonRequired {
      def compileOptional[A](field: Field[?, A]): ShouldSkip[A] = { a =>
        a == None
      }
    }

  val SkipIfEmptyOrDefaultOptionals: FieldSkipCompiler =
    SkipIfEmptyOptionals combine SkipIfDefaultOptionals
}
