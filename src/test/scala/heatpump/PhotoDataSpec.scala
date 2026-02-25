package heatpump

import java.time.LocalDate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PhotoDataSpec extends AnyFlatSpec with Matchers {

  "PhotoType.fromString" should "match exact labels" in {
    PhotoType.fromString("Consommation d'énergie") shouldBe Some(PhotoType.ConsommationEnergie)
    PhotoType.fromString("Chauffage appoint élec.") shouldBe Some(PhotoType.ChauffageAppointElec)
    PhotoType.fromString("Compresseur") shouldBe Some(PhotoType.Compresseur)
    PhotoType.fromString("Energie fournie") shouldBe Some(PhotoType.EnergieFournie)
  }

  it should "match with flexible patterns" in {
    PhotoType.fromString("Consommation d'énergie totale") shouldBe Some(PhotoType.ConsommationEnergie)
    PhotoType.fromString("Chauffage appoint électrique") shouldBe Some(PhotoType.ChauffageAppointElec)
    PhotoType.fromString("Compresseur kWh") shouldBe Some(PhotoType.Compresseur)
    PhotoType.fromString("Energie fournie Total") shouldBe Some(PhotoType.EnergieFournie)
  }

  it should "be case-insensitive" in {
    PhotoType.fromString("COMPRESSEUR") shouldBe Some(PhotoType.Compresseur)
    PhotoType.fromString("energie fournie") shouldBe Some(PhotoType.EnergieFournie)
  }

  it should "return None for unrecognized types" in {
    PhotoType.fromString("Unknown screen") shouldBe None
    PhotoType.fromString("") shouldBe None
  }

  "HeatPumpReading.fromPhotos" should "correctly assemble a reading from 4 photos" in {
    val photos = List(
      ExtractedPhoto(PhotoType.ConsommationEnergie, Map("Total" -> 7325)),
      ExtractedPhoto(PhotoType.ChauffageAppointElec, Map("Chauff." -> 0, "Eau chaude sanitaire" -> 5)),
      ExtractedPhoto(PhotoType.Compresseur, Map("Total" -> 7320, "Chauff." -> 4417, "Eau chaude sanitaire" -> 2559, "Refroid." -> 334)),
      ExtractedPhoto(PhotoType.EnergieFournie, Map("Total" -> 25280, "Chauffage" -> 15738, "ECS" -> 7048, "Refroid." -> 2496))
    )
    val date = LocalDate.of(2024, 6, 15)

    val result = HeatPumpReading.fromPhotos(photos, date)
    result shouldBe a[Right[_, _]]

    val reading = result.toOption.get
    reading.date shouldBe date
    reading.consommationTotale shouldBe 7325
    reading.chauffageAppointChauff shouldBe 0
    reading.chauffageAppointECS shouldBe 5
    reading.compresseurTotal shouldBe 7320
    reading.compresseurChauff shouldBe 4417
    reading.compresseurECS shouldBe 2559
    reading.compresseurRefroid shouldBe 334
    reading.energieFournieTotale shouldBe 25280
    reading.energieFournieChauffage shouldBe 15738
    reading.energieFournieECS shouldBe 7048
    reading.energieFournieRefroid shouldBe 2496
  }

  it should "return an error when a photo type is missing" in {
    val photos = List(
      ExtractedPhoto(PhotoType.ConsommationEnergie, Map("Total" -> 7325)),
      ExtractedPhoto(PhotoType.ChauffageAppointElec, Map("Chauff." -> 0, "Eau chaude sanitaire" -> 5)),
      ExtractedPhoto(PhotoType.Compresseur, Map("Total" -> 7320, "Chauff." -> 4417, "Eau chaude sanitaire" -> 2559, "Refroid." -> 334))
      // Missing EnergieFournie
    )
    val date = LocalDate.of(2024, 6, 15)

    val result = HeatPumpReading.fromPhotos(photos, date)
    result shouldBe a[Left[_, _]]
    result.left.toOption.get should include("Missing photo")
  }

  it should "return an error when a required value is missing" in {
    val photos = List(
      ExtractedPhoto(PhotoType.ConsommationEnergie, Map("Total" -> 7325)),
      ExtractedPhoto(PhotoType.ChauffageAppointElec, Map("Chauff." -> 0, "Eau chaude sanitaire" -> 5)),
      ExtractedPhoto(PhotoType.Compresseur, Map("Total" -> 7320, "Chauff." -> 4417, "Eau chaude sanitaire" -> 2559)),
      // Missing Refroid. in Compresseur
      ExtractedPhoto(PhotoType.EnergieFournie, Map("Total" -> 25280, "Chauffage" -> 15738, "ECS" -> 7048, "Refroid." -> 2496))
    )
    val date = LocalDate.of(2024, 6, 15)

    val result = HeatPumpReading.fromPhotos(photos, date)
    result shouldBe a[Left[_, _]]
    result.left.toOption.get should include("Missing value")
  }

  "HeatPumpReading.toRow" should "produce correct column values" in {
    val reading = HeatPumpReading(
      date = LocalDate.of(2024, 6, 15),
      consommationTotale = 7325,
      chauffageAppointChauff = 0,
      chauffageAppointECS = 5,
      compresseurTotal = 7320,
      compresseurChauff = 4417,
      compresseurECS = 2559,
      compresseurRefroid = 334,
      energieFournieTotale = 25280,
      energieFournieChauffage = 15738,
      energieFournieECS = 7048,
      energieFournieRefroid = 2496
    )

    reading.toRow shouldBe List(
      "2024-06-15", "7325", "0", "5", "7320", "4417", "2559", "334",
      "25280", "15738", "7048", "2496"
    )
  }

  "HeatPumpReading.fromRow" should "parse a row from the spreadsheet" in {
    val row = List("2024-06-15", "7325", "0", "5", "7320", "4417", "2559", "334", "25280", "15738", "7048", "2496")
    val result = HeatPumpReading.fromRow(row)
    result shouldBe defined
    result.get.consommationTotale shouldBe 7325
    result.get.date shouldBe LocalDate.of(2024, 6, 15)
  }

  it should "handle comma-formatted numbers" in {
    val row = List("2024-06-15", "7,325", "0", "5", "7,320", "4,417", "2,559", "334", "25,280", "15,738", "7,048", "2,496")
    val result = HeatPumpReading.fromRow(row)
    result shouldBe defined
    result.get.consommationTotale shouldBe 7325
    result.get.energieFournieTotale shouldBe 25280
  }

  it should "return None for rows with too few columns" in {
    HeatPumpReading.fromRow(List("2024-06-15", "7325")) shouldBe None
  }

  it should "return None for rows with invalid data" in {
    val row = List("not-a-date", "7325", "0", "5", "7320", "4417", "2559", "334", "25280", "15738", "7048", "2496")
    HeatPumpReading.fromRow(row) shouldBe None
  }
}
