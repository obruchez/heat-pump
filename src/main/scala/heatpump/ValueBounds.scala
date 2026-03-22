package heatpump

import java.time.LocalDate
import java.time.temporal.ChronoUnit

/** Bounds for values in a specific photo type. */
case class PhotoBounds(lower: Map[String, Int], upper: Map[String, Int])

object ValueBounds {

  /** Maps a HeatPumpReading to the label->value map for a given photo type. */
  def valuesForPhotoType(reading: HeatPumpReading, photoType: PhotoType): Map[String, Int] = photoType match {
    case PhotoType.ConsommationEnergie => Map("Total" -> reading.consommationTotale)
    case PhotoType.ChauffageAppointElec => Map(
      "Chauff." -> reading.chauffageAppointChauff,
      "Eau chaude sanitaire" -> reading.chauffageAppointECS
    )
    case PhotoType.Compresseur => Map(
      "Total" -> reading.compresseurTotal,
      "Chauff." -> reading.compresseurChauff,
      "Eau chaude sanitaire" -> reading.compresseurECS,
      "Refroid." -> reading.compresseurRefroid
    )
    case PhotoType.EnergieFournie => Map(
      "Total" -> reading.energieFournieTotale,
      "Chauffage" -> reading.energieFournieChauffage,
      "ECS" -> reading.energieFournieECS,
      "Refroid." -> reading.energieFournieRefroid
    )
  }

  /**
   * Computes bounds per photo type based on historical data.
   * Lower bounds come from lastRow values (cumulative counters only go up).
   * Upper bounds are extrapolated from the daily rate between secondToLastRow and lastRow,
   * multiplied by a safety factor.
   */
  def compute(
      lastRow: HeatPumpReading,
      secondToLastRow: Option[HeatPumpReading],
      newDate: LocalDate,
      factor: Double,
      minDailyChange: Double
  ): Map[PhotoType, PhotoBounds] = {
    PhotoType.all.map { pt =>
      val lastValues = valuesForPhotoType(lastRow, pt)

      val upperValues = secondToLastRow match {
        case Some(prev) =>
          val daysBetween = ChronoUnit.DAYS.between(prev.date, lastRow.date).toDouble
          val daysSinceLast = ChronoUnit.DAYS.between(lastRow.date, newDate).toDouble

          if (daysBetween <= 0 || daysSinceLast <= 0) Map.empty[String, Int]
          else {
            val prevValues = valuesForPhotoType(prev, pt)
            lastValues.map { case (key, lastVal) =>
              val prevVal = prevValues.getOrElse(key, lastVal)
              val dailyRate = math.max((lastVal - prevVal).toDouble / daysBetween, minDailyChange)
              val upperBound = math.ceil(lastVal + dailyRate * daysSinceLast * factor).toInt
              key -> upperBound
            }
          }
        case None => Map.empty[String, Int]
      }

      pt -> PhotoBounds(lower = lastValues, upper = upperValues)
    }.toMap
  }

  /**
   * Formats bounds as a string section for the Claude API prompt.
   */
  def formatForPrompt(bounds: Map[PhotoType, PhotoBounds]): String = {
    val sections = PhotoType.all.flatMap { pt =>
      bounds.get(pt).map { pb =>
        val ranges = pb.lower.keys.toList.sorted.map { key =>
          val lower = pb.lower.get(key)
          val upper = pb.upper.get(key)
          (lower, upper) match {
            case (Some(lb), Some(ub)) => s"  $key: between $lb and $ub"
            case (Some(lb), None)     => s"  $key: at least $lb"
            case _                    => s"  $key: no bounds"
          }
        }.mkString("\n")
        s"For \"${pt.label}\":\n$ranges"
      }
    }

    s"""Expected value ranges based on historical data (values are cumulative kWh counters that only increase):
       |${sections.mkString("\n")}
       |These ranges are guidelines to help validate your OCR readings. Report the actual values you see, even if they fall outside these ranges.""".stripMargin
  }

  /** Prints computed bounds to stdout in a readable format. */
  def printBounds(bounds: Map[PhotoType, PhotoBounds]): Unit = {
    println("Computed value bounds:")
    PhotoType.all.foreach { pt =>
      bounds.get(pt).foreach { pb =>
        println(s"  ${pt.label}:")
        pb.lower.keys.toList.sorted.foreach { key =>
          val lower = pb.lower.get(key)
          val upper = pb.upper.get(key)
          val range = (lower, upper) match {
            case (Some(lb), Some(ub)) => s"$lb .. $ub"
            case (Some(lb), None)     => s">= $lb"
            case _                    => "?"
          }
          println(s"    %-25s %s".format(key, range))
        }
      }
    }
  }

  /**
   * Checks a reading against bounds and returns warnings keyed by field identifier.
   */
  def checkReading(
      reading: HeatPumpReading,
      bounds: Map[PhotoType, PhotoBounds]
  ): Map[String, String] = {
    val checks: List[(String, PhotoType, String, Int)] = List(
      ("consommationTotale", PhotoType.ConsommationEnergie, "Total", reading.consommationTotale),
      ("chauffageAppointChauff", PhotoType.ChauffageAppointElec, "Chauff.", reading.chauffageAppointChauff),
      ("chauffageAppointECS", PhotoType.ChauffageAppointElec, "Eau chaude sanitaire", reading.chauffageAppointECS),
      ("compresseurTotal", PhotoType.Compresseur, "Total", reading.compresseurTotal),
      ("compresseurChauff", PhotoType.Compresseur, "Chauff.", reading.compresseurChauff),
      ("compresseurECS", PhotoType.Compresseur, "Eau chaude sanitaire", reading.compresseurECS),
      ("compresseurRefroid", PhotoType.Compresseur, "Refroid.", reading.compresseurRefroid),
      ("energieFournieTotale", PhotoType.EnergieFournie, "Total", reading.energieFournieTotale),
      ("energieFournieChauffage", PhotoType.EnergieFournie, "Chauffage", reading.energieFournieChauffage),
      ("energieFournieECS", PhotoType.EnergieFournie, "ECS", reading.energieFournieECS),
      ("energieFournieRefroid", PhotoType.EnergieFournie, "Refroid.", reading.energieFournieRefroid)
    )

    checks.flatMap { case (fieldId, pt, label, value) =>
      bounds.get(pt).flatMap { pb =>
        val warnings = List(
          pb.lower.get(label).filter(lb => value < lb).map(lb => s"below lower bound $lb"),
          pb.upper.get(label).filter(ub => value > ub).map(ub => s"above upper bound $ub")
        ).flatten

        if (warnings.nonEmpty) Some(fieldId -> warnings.mkString(", "))
        else None
      }
    }.toMap
  }
}
