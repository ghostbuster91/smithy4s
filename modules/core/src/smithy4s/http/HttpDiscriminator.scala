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

package smithy4s.http

import smithy4s.ShapeId

sealed trait HttpDiscriminator extends Product with Serializable

object HttpDiscriminator {

  // format: off
  final case class FullId(shapeId: ShapeId) extends HttpDiscriminator
  final case class NameOnly(name: String) extends HttpDiscriminator
  final case class StatusCode(int: Int) extends HttpDiscriminator
  case object Undetermined extends HttpDiscriminator
  // format: on

  def fromResponse(
      discriminatingHeaderNames: List[String],
      response: HttpResponse[Any]
  ): HttpDiscriminator =
    fromStatusOrHeader(
      discriminatingHeaderNames,
      response.statusCode,
      response.headers
    )

  def fromMetadata(
      discriminatingHeaderNames: List[String],
      metadata: Metadata
  ): Option[HttpDiscriminator] = {
    metadata.statusCode.map(code =>
      fromStatusOrHeader(discriminatingHeaderNames, code, metadata.headers)
    )
  }

  def fromStatusOrHeader(
      discriminatingHeaderNames: List[String],
      statusCode: Int,
      headers: Map[CaseInsensitive, Seq[String]]
  ): HttpDiscriminator = fromStatusOrHeaderWithResults(
    discriminatingHeaderNames,
    statusCode,
    headers
  ).discriminator

  def fromStatusOrHeaderWithResults(
      discriminatingHeaderNames: List[String],
      statusCode: Int,
      headers: Map[CaseInsensitive, Seq[String]]
  ): HttpDiscriminatorMatchResults = {
    val (reasons, discriminator) = discriminatingHeaderNames
      .map(CaseInsensitive(_))
      .foldLeft((List.empty[String], Option.empty[HttpDiscriminator])) {
        (acc, item) =>
          acc match {
            case (info, None) =>
              val header = headers
                .get(item)
                .flatMap(_.headOption)
              header match {
                case Some(errorType) =>
                  (
                    info,
                    Some(
                      ShapeId
                        .parse(errorType)
                        .map(FullId(_))
                        .getOrElse(NameOnly(errorType))
                    )
                  )
                case None =>
                  (info :+ s"Cannot discriminate error by $item header due to missing response header", None)
              }
            case (reasons, header) => (reasons, header)
          }
      }

    HttpDiscriminatorMatchResults(reasons, discriminator.getOrElse(StatusCode(statusCode)))
  }

}
