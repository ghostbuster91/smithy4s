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

trait FieldRenderPredicateCompiler { self =>
  def compile[A](
      field: Field[?, A]
  ): FieldRenderPredicateCompiler.ShouldRender[A]

  def &&(other: FieldRenderPredicateCompiler): FieldRenderPredicateCompiler =
    new FieldRenderPredicateCompiler {

      def compile[A](
          field: Field[?, A]
      ): FieldRenderPredicateCompiler.ShouldRender[A] = {
        val r1 = self.compile(field)
        val r2 = other.compile(field)
        a => r1(a) && r2(a)
      }
    }
  def ||(other: FieldRenderPredicateCompiler): FieldRenderPredicateCompiler =
    new FieldRenderPredicateCompiler {

      def compile[A](
          field: Field[?, A]
      ): FieldRenderPredicateCompiler.ShouldRender[A] = {
        val r1 = self.compile(field)
        val r2 = other.compile(field)
        a => r1(a) || r2(a)
      }
    }
}

object FieldRenderPredicateCompiler {

  type ShouldRender[A] = A => Boolean

  trait OptionalPredicate extends FieldRenderPredicateCompiler {

    final def compile[A](
        field: Field[?, A]
    ): FieldRenderPredicateCompiler.ShouldRender[A] = {
      if (field.isRequired) Function.const(true)
      else compileOptional(field)

    }

    def compileOptional[A](
        field: Field[?, A]
    ): FieldRenderPredicateCompiler.ShouldRender[A]

  }

  val AlwaysRender = new FieldRenderPredicateCompiler {
    def compile[A](field: Field[_, A]): ShouldRender[A] = Function.const(true)
  }

  val IsNotDefaultOptional =
    new FieldRenderPredicateCompiler.OptionalPredicate {
      def compileOptional[A](field: Field[?, A]): ShouldRender[A] = {
        // Optional fields have None as their default, so we need to make sure not to skip them here
        a => a == None || !field.isDefaultValue(a)
      }
    }

  val IsNotAnEmptyOptional =
    new FieldRenderPredicateCompiler.OptionalPredicate {
      def compileOptional[A](field: Field[?, A]): ShouldRender[A] = { a =>
        a != None
      }
    }

  val IsNeitherEmptyOptionalNorDefaultOptional =
    IsNotAnEmptyOptional && IsNotDefaultOptional

  def fromExplicitDefaults(
      explicitDefaultsEncoding: Boolean
  ): FieldRenderPredicateCompiler = {
    if (explicitDefaultsEncoding) AlwaysRender
    else IsNeitherEmptyOptionalNorDefaultOptional
  }
}
