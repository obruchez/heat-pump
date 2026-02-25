package heatpump

import java.time.LocalDate
import java.time.format.DateTimeFormatter

/** The four types of photos taken of the heat pump LCD screen. */
sealed trait PhotoType {
  def label: String
}

object PhotoType {
  case object ConsommationEnergie     extends PhotoType { val label = "Consommation d'énergie" }
  case object ChauffageAppointElec    extends PhotoType { val label = "Chauffage appoint élec." }
  case object Compresseur             extends PhotoType { val label = "Compresseur" }
  case object EnergieFournie          extends PhotoType { val label = "Energie fournie" }

  val all: List[PhotoType] = List(ConsommationEnergie, ChauffageAppointElec, Compresseur, EnergieFournie)

  def fromString(s: String): Option[PhotoType] = {
    val normalized = s.trim.toLowerCase
    all.find(_.label.toLowerCase == normalized)
      .orElse {
        // Flexible matching for common variations
        if (normalized.contains("consommation") && normalized.contains("nergie")) Some(ConsommationEnergie)
        else if (normalized.contains("chauffage") && normalized.contains("appoint")) Some(ChauffageAppointElec)
        else if (normalized.contains("compresseur")) Some(Compresseur)
        else if (normalized.contains("fournie")) Some(EnergieFournie)
        else None
      }
  }
}

/**
 * Response from Claude API for a single photo.
 * Values map: label -> value (None if unreadable).
 */
case class ClaudePhotoResponse(
    photoType: String,
    values: Map[String, Option[Int]],
    confident: Boolean
)

/**
 * Extracted and validated data from a single photo.
 */
case class ExtractedPhoto(
    photoType: PhotoType,
    values: Map[String, Int]
)

/**
 * Complete reading from all 4 photos, ready for spreadsheet insertion.
 * Column mapping (B through L):
 *   B = consommationTotale         (Photo 1: Total)
 *   C = chauffageAppointChauff     (Photo 2: Chauff.)
 *   D = chauffageAppointECS        (Photo 2: Eau chaude sanitaire)
 *   E = compresseurTotal           (Photo 3: Total)
 *   F = compresseurChauff          (Photo 3: Chauff.)
 *   G = compresseurECS             (Photo 3: Eau chaude sanitaire)
 *   H = compresseurRefroid         (Photo 3: Refroid.)
 *   I = energieFournieTotale       (Photo 4: Total)
 *   J = energieFournieChauffage    (Photo 4: Chauffage)
 *   K = energieFournieECS          (Photo 4: ECS)
 *   L = energieFournieRefroid      (Photo 4: Energie de ref. fournie)
 */
case class HeatPumpReading(
    date: LocalDate,
    consommationTotale: Int,
    chauffageAppointChauff: Int,
    chauffageAppointECS: Int,
    compresseurTotal: Int,
    compresseurChauff: Int,
    compresseurECS: Int,
    compresseurRefroid: Int,
    energieFournieTotale: Int,
    energieFournieChauffage: Int,
    energieFournieECS: Int,
    energieFournieRefroid: Int
) {
  /** Returns the row as a list of strings for Google Sheets (columns A through L). */
  def toRow: List[String] = List(
    date.format(DateTimeFormatter.ISO_LOCAL_DATE),
    consommationTotale.toString,
    chauffageAppointChauff.toString,
    chauffageAppointECS.toString,
    compresseurTotal.toString,
    compresseurChauff.toString,
    compresseurECS.toString,
    compresseurRefroid.toString,
    energieFournieTotale.toString,
    energieFournieChauffage.toString,
    energieFournieECS.toString,
    energieFournieRefroid.toString
  )
}

object HeatPumpReading {
  /**
   * Assembles a HeatPumpReading from 4 ExtractedPhoto objects.
   * Returns Left with a message if any required photo type is missing or values are incomplete.
   */
  def fromPhotos(photos: List[ExtractedPhoto], date: LocalDate): Either[String, HeatPumpReading] = {
    def findPhoto(pt: PhotoType): Either[String, ExtractedPhoto] =
      photos.find(_.photoType == pt).toRight(s"Missing photo: ${pt.label}")

    def getValue(photo: ExtractedPhoto, key: String): Either[String, Int] =
      photo.values.get(key).toRight(s"Missing value '$key' in ${photo.photoType.label}")

    // Flexible key matching: find the best match for a key in a photo's values
    def findValue(photo: ExtractedPhoto, candidates: List[String]): Either[String, Int] = {
      val normalizedValues = photo.values.map { case (k, v) => k.trim.toLowerCase -> v }
      candidates.map(_.toLowerCase)
        .collectFirst { case c if normalizedValues.contains(c) => normalizedValues(c) }
        .toRight(s"Missing value (tried: ${candidates.mkString(", ")}) in ${photo.photoType.label}")
    }

    for {
      p1 <- findPhoto(PhotoType.ConsommationEnergie)
      p2 <- findPhoto(PhotoType.ChauffageAppointElec)
      p3 <- findPhoto(PhotoType.Compresseur)
      p4 <- findPhoto(PhotoType.EnergieFournie)

      consommationTotale      <- findValue(p1, List("Total"))
      chauffageAppointChauff  <- findValue(p2, List("Chauff.", "Chauffage"))
      chauffageAppointECS     <- findValue(p2, List("Eau chaude sanitaire", "ECS"))
      compresseurTotal        <- findValue(p3, List("Total"))
      compresseurChauff       <- findValue(p3, List("Chauff.", "Chauffage"))
      compresseurECS          <- findValue(p3, List("Eau chaude sanitaire", "ECS"))
      compresseurRefroid      <- findValue(p3, List("Refroid.", "Refroidissement"))
      energieFournieTotale    <- findValue(p4, List("Total", "Energie fournie Total"))
      energieFournieChauffage <- findValue(p4, List("Chauffage", "Energie fournie Chauffage"))
      energieFournieECS       <- findValue(p4, List("ECS", "Energie fournie ECS"))
      energieFournieRefroid   <- findValue(p4, List("Refroid.", "Refroidissement", "Energie de ref. fournie"))
    } yield HeatPumpReading(
      date = date,
      consommationTotale = consommationTotale,
      chauffageAppointChauff = chauffageAppointChauff,
      chauffageAppointECS = chauffageAppointECS,
      compresseurTotal = compresseurTotal,
      compresseurChauff = compresseurChauff,
      compresseurECS = compresseurECS,
      compresseurRefroid = compresseurRefroid,
      energieFournieTotale = energieFournieTotale,
      energieFournieChauffage = energieFournieChauffage,
      energieFournieECS = energieFournieECS,
      energieFournieRefroid = energieFournieRefroid
    )
  }

  /** Parses a row from Google Sheets back into a HeatPumpReading (for duplicate detection). */
  def fromRow(row: List[String]): Option[HeatPumpReading] = {
    if (row.size < 12) return None
    try {
      Some(HeatPumpReading(
        date = LocalDate.parse(row(0), DateTimeFormatter.ISO_LOCAL_DATE),
        consommationTotale = parseIntValue(row(1)),
        chauffageAppointChauff = parseIntValue(row(2)),
        chauffageAppointECS = parseIntValue(row(3)),
        compresseurTotal = parseIntValue(row(4)),
        compresseurChauff = parseIntValue(row(5)),
        compresseurECS = parseIntValue(row(6)),
        compresseurRefroid = parseIntValue(row(7)),
        energieFournieTotale = parseIntValue(row(8)),
        energieFournieChauffage = parseIntValue(row(9)),
        energieFournieECS = parseIntValue(row(10)),
        energieFournieRefroid = parseIntValue(row(11))
      ))
    } catch {
      case _: Exception => None
    }
  }

  /** Parse a string that may contain locale formatting (e.g., "7,091" or "7091"). */
  private def parseIntValue(s: String): Int =
    s.trim.replaceAll("[,\\s]", "").toInt
}
