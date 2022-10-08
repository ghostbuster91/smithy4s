/*
 *  Copyright 2021-2022 Disney Streaming
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

package smithy4s.xml
package internals

import cats.syntax.all._

import smithy4s.xml.XmlDocument.XmlContent
import smithy4s.xml.XmlDocument.XmlQName
import smithy4s.xml.XmlDocument.XmlElem
import smithy4s.xml.XmlDocument.XmlText
import smithy4s.xml.XmlDocument.XmlAttr
import cats.MonoidK
import smithy4s.capability.EncoderK

/**
  * This constructs allow for decoding XML data. It is not limited to top-level
  * documents, and works against XmlCursor. Smithy4s does not have vocation to be
  * a general-purpose XML library, so we keep this as a private implementation detail
  * that should never be used directly.
  *
  * It exposes simple combinators that allow to indicate that the decoding should happen
  * further down the cursor, or enable the prevention of failure when decoding an absence
  * of node (which is the case for optional data).
  */
private[smithy4s] trait XmlEncoder[-A] { self =>
  def encode(value: A): List[XmlContent]
  def encodesUnion: Boolean = false

  def contramap[B](f: B => A): XmlEncoder[B] = new XmlEncoder[B] {
    def encode(value: B): List[XmlContent] = self.encode(f(value))
    override def encodesUnion: Boolean = self.encodesUnion
    override def down(tag: XmlQName): XmlEncoder[B] =
      self.down(tag).contramap(f)
    override def optional: XmlEncoder[Option[B]] =
      self.optional.contramap[Option[B]](_.map(f))
    override def attribute(name: XmlQName): XmlEncoder[B] =
      self.attribute(name).contramap(f)
  }

  def attribute(name: XmlQName): XmlEncoder[A] = { a =>
    val values = self.encode(a).collect { case text @ XmlText(_) => text }
    List(XmlAttr(name, values))
  }

  def optional: XmlEncoder[Option[A]] = {
    (_: Option[A]) match {
      case None    => Nil
      case Some(a) => self.encode(a)
    }
  }

  def down(tag: XmlQName): XmlEncoder[A] = { a =>
    val content = self.encode(a)
    val (attributes, children) = content.partitionEither {
      case attr @ XmlAttr(_, _) => Left(attr)
      case other                => Right(other)
    }
    List(XmlElem(tag, attributes, children))
  }

}

object XmlEncoder {
  implicit val xmlEncoderK: EncoderK[XmlEncoder, List[XmlContent]] =
    new EncoderK[XmlEncoder, List[XmlContent]] {
      def apply[A](fa: XmlEncoder[A], a: A): List[XmlContent] = fa.encode(a)

      def absorb[A](f: A => List[XmlContent]): XmlEncoder[A] =
        new XmlEncoder[A] {
          def encode(value: A): List[XmlContent] = f(value)
        }
    }

  val nil: XmlEncoder[Any] = { (_: Any) => Nil }

  implicit val monoidK: MonoidK[XmlEncoder] = new MonoidK[XmlEncoder] {
    def combineK[A](x: XmlEncoder[A], y: XmlEncoder[A]): XmlEncoder[A] =
      new XmlEncoder[A] {
        def encode(value: A): List[XmlContent] =
          x.encode(value) ++ y.encode(value)
      }
    def empty[A]: XmlEncoder[A] = nil
  }
}
