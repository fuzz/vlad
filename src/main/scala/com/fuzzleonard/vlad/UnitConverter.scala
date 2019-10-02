package com.fuzzleonard.vlad

import cats.Applicative
import cats.implicits._
import fastparse._, NoWhitespace._
import io.circe.{Encoder, Json}
import org.http4s.EntityEncoder
import org.http4s.circe._
import scala.math.{Pi, ceil, log10, pow, round}

trait UnitConverter[F[_]] {
  def convertUnits(u: UnitConverter.UnitString): F[UnitConverter.Converter]
}

object UnitConverter {
  implicit def apply[F[_]](implicit ev: UnitConverter[F]): UnitConverter[F] = ev

  final case class UnitString(unitString: String) extends AnyVal

  final case class Converter(s: (String, String)) extends AnyVal

  object Converter {
    implicit val converterEncoder: Encoder[Converter] = new Encoder[Converter] {
      final def apply(a: Converter): Json = Json.obj(
        ("unit_name", Json.fromString(a.s._1)),
        ("multiplication_factor", Json.fromString(a.s._2))
      )
    }

    implicit def converterEntityEncoder[F[_] : Applicative]: EntityEncoder[F, Converter] =
      jsonEncoderOf[F, Converter]
  }

  def impl[F[_] : Applicative]: UnitConverter[F] = new UnitConverter[F] {
    def convertUnits(u: UnitConverter.UnitString): F[UnitConverter.Converter] = {
      val (s, d) = fastparse.parse(u.unitString, unitParser.parser(_)).get.value.value

      // A decimal representation per the example request
      val mf = s"%.${unitParser.Precision}f".format(d).toString
        .replaceAll("""0+$""", "")
        .replaceAll("""\.$""", ".0")

      // Use this instead for proper sigfigs via scientific notation
      // Converter(us, d.toString).pure[F]
      Converter(s, mf).pure[F]
    }
  }
}

object unitParser {
  val Precision = 14

  case class SciUnit(value: (String, Double)) extends AnyVal

  def unit[_: P]: P[SciUnit] = P(StringIn(
    "°", "'", "\"", "L", "arcminute", "arcsecond", "day", "degree", "d",
    "ha", "hectare", "hour", "h", "kilogram", "kg", "litre", "metre",
    "minute", "min", "m", "radian", "rad", "second", "s", "tonne", "t"
  )).!.map {
    //   Name        | Symbol=> SI Conversion
    case "arcminute" | "'"   => SciUnit("rad", Pi / 10800)
    case "arcsecond" | "\""  => SciUnit("rad", Pi / 648000)
    case "day"       | "d"   => SciUnit("s"  , 86400)
    case "degree"    | "°"   => SciUnit("rad", Pi / 180)
    case "hectare"   | "ha"  => SciUnit("m"  , pow(10000, 2))
    case "hour"      | "h"   => SciUnit("s"  , 3600)
    case "litre"     | "L"   => SciUnit("m"  , pow(0.001, 3))
    case "minute"    | "min" => SciUnit("s"  , 60)
    case "tonne"     | "t"   => SciUnit("kg" , 1000)
    case "kilogram"  | "kg"  => SciUnit("kg" , 1)
    case "metre"     | "m"   => SciUnit("m"  , 1)
    case "radian"    | "rad" => SciUnit("rad", 1)
    case "second"    | "s"   => SciUnit("s"  , 1)
  }

  def parens[_: P]: P[SciUnit] = P("(" ~/ operat ~ ")")

  def factor[_: P]: P[SciUnit] = P(unit | parens)

  def operat[_: P]: P[SciUnit] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)

  def parser[_: P]: P[SciUnit] = P(operat ~ End).map(sigFig).map(stripOuterParens)

  def eval(tree: (SciUnit, Seq[(String, SciUnit)])): SciUnit = {
    val (base, ops) = tree
    val t = ops.foldLeft(base) { case (lhs, (op, rhs)) => op match {
      case "*" => SciUnit(lhs.value._1 + "*" + rhs.value._1, lhs.value._2 * rhs.value._2)
      case "/" => SciUnit(lhs.value._1 + "/" + rhs.value._1, lhs.value._2 / rhs.value._2)
    }}
    SciUnit("(" + t.value._1 + ")", t.value._2)
  }

  def sigFig(s: SciUnit): SciUnit = {
    val power = Precision - ceil(log10(s.value._2)).toInt
    val magnitude = pow(10, power)
    val shifted = round(s.value._2 * magnitude)

    SciUnit(s.value._1, shifted / magnitude)
  }

  def stripOuterParens(s: SciUnit): SciUnit = {
    val a = s.value._1.replaceAll("""^\(""", "")
      .replaceAll("""\)$""", "")

    SciUnit(a, s.value._2)
  }
}
