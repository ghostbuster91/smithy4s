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

abstract class ValidatedNewtype[A] extends AbstractNewtype[A] { self =>

  @inline def apply(a: A): Either[String, Type]

  @inline final def unsafeApply(a: A): Type = apply(a) match {
    case Right(value) => value
    case Left(error)  => throw new IllegalArgumentException(error)
  }

  implicit final class Ops(val self: Type) {
    @inline final def value: A = ValidatedNewtype.this.value(self)
  }

  def unapply(t: Type): Some[A] = Some(t.value)

  object hint {
    def unapply(h: Hints): Option[Type] = h.get(tag)
  }
}

object ValidatedNewtype {
  private[smithy4s] trait Make[A, B] extends Bijection[A, B]
}
