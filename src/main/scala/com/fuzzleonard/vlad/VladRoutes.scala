package com.fuzzleonard.vlad

import cats.effect.Sync
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

object VladRoutes {
  def unitConverterRoutes[F[_]: Sync](U: UnitConverter[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._ //unitString
    object units extends QueryParamDecoderMatcher[String]("units")
    HttpRoutes.of[F] {
      case GET -> Root / "units" / "si" :? units(unitString) =>
        for {
          conversion <- U.convertUnits(UnitConverter.UnitString(unitString))
          resp <- Ok(conversion)
        } yield resp
    }
  }
}