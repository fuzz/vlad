package com.fuzzleonard.vlad

import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import cats.implicits._
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import scala.concurrent.ExecutionContext.global

object VladServer {
  def stream[F[_]: ConcurrentEffect](implicit T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = {
    for {
      client <- BlazeClientBuilder[F](global).stream
      unitConverterAlg = UnitConverter.impl[F]

      httpApp = (
        VladRoutes.unitConverterRoutes[F](unitConverterAlg)
      ).orNotFound

      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(1337, "127.0.0.1")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}