package com.fuzzleonard.vlad

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult

class UnitConverterSpec extends org.specs2.mutable.Specification {

  "UnitConverter" >> {
    "handle test prompt case" >> {
      uriReturnsTestPrompt()
    }
    "handle complex test case" >> {
      uriReturnsComplex()
    }
  }

  private[this] val retTestPrompt: Response[IO] = {
    val getUC = Request[IO](Method.GET, uri"/units/si?units=(degree/minute)")
    val unitConverter = UnitConverter.impl[IO]
    VladRoutes.unitConverterRoutes(unitConverter).orNotFound(getUC).unsafeRunSync()
  }

  private[this] def uriReturnsTestPrompt(): MatchResult[String] =
    retTestPrompt.as[String].unsafeRunSync() must beEqualTo("{\"unit_name\":\"(rad/s)\",\"multiplication_factor\":\"0.00029088820867\"}")

  private[this] val retComplex: Response[IO] = {
    val getNP = Request[IO](Method.GET, uri"/units/si?units=degree/minute*ha*((hectare*%22)*%C2%B0)")
    val unitConverter = UnitConverter.impl[IO]
    VladRoutes.unitConverterRoutes(unitConverter).orNotFound(getNP).unsafeRunSync()
  }

  private[this] def uriReturnsComplex(): MatchResult[String] =
    retComplex.as[String].unsafeRunSync() must beEqualTo("{\"unit_name\":\"rad/s*m*((m*rad)*rad)\",\"multiplication_factor\":\"246137.82102814\"}")

}