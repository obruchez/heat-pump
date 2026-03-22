package heatpump

import java.time.LocalDate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValueBoundsSpec extends AnyFlatSpec with Matchers {

  private val reading1 = HeatPumpReading(
    date = LocalDate.of(2024, 5, 15),
    consommationTotale = 7000,
    chauffageAppointChauff = 0,
    chauffageAppointECS = 5,
    compresseurTotal = 6995,
    compresseurChauff = 4200,
    compresseurECS = 2450,
    compresseurRefroid = 300,
    energieFournieTotale = 24000,
    energieFournieChauffage = 15000,
    energieFournieECS = 6700,
    energieFournieRefroid = 2300
  )

  private val reading2 = HeatPumpReading(
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

  private val newDate = LocalDate.of(2024, 7, 15)

  "ValueBounds.compute" should "use last row values as lower bounds" in {
    val bounds = ValueBounds.compute(reading2, None, newDate, 2.0, 1.0)

    bounds(PhotoType.ConsommationEnergie).lower shouldBe Map("Total" -> 7325)
    bounds(PhotoType.ChauffageAppointElec).lower shouldBe Map("Chauff." -> 0, "Eau chaude sanitaire" -> 5)
    bounds(PhotoType.Compresseur).lower shouldBe Map(
      "Total" -> 7320, "Chauff." -> 4417, "Eau chaude sanitaire" -> 2559, "Refroid." -> 334
    )
    bounds(PhotoType.EnergieFournie).lower shouldBe Map(
      "Total" -> 25280, "Chauffage" -> 15738, "ECS" -> 7048, "Refroid." -> 2496
    )
  }

  it should "have empty upper bounds when no second-to-last row is available" in {
    val bounds = ValueBounds.compute(reading2, None, newDate, 2.0, 1.0)

    bounds(PhotoType.ConsommationEnergie).upper shouldBe empty
    bounds(PhotoType.Compresseur).upper shouldBe empty
  }

  it should "compute upper bounds from two rows with linear extrapolation" in {
    // reading1 (May 15) -> reading2 (Jun 15): 31 days
    // newDate (Jul 15) is 30 days after reading2
    // ConsommationEnergie Total: 7000 -> 7325, diff = 325, daily rate = 325/31 ≈ 10.48
    // upper = 7325 + ceil(10.48 * 30 * 2.0) = 7325 + ceil(629.03) = 7325 + 630 = 7955
    val bounds = ValueBounds.compute(reading2, Some(reading1), newDate, 2.0, 1.0)

    val upperTotal = bounds(PhotoType.ConsommationEnergie).upper("Total")
    upperTotal shouldBe 7955
  }

  it should "use minDailyChange when actual daily rate is zero" in {
    // ChauffageAppointElec Chauff.: 0 -> 0, daily rate = 0, but minDailyChange = 1.0
    // upper = 0 + ceil(1.0 * 30 * 2.0) = 60
    val bounds = ValueBounds.compute(reading2, Some(reading1), newDate, 2.0, 1.0)
    bounds(PhotoType.ChauffageAppointElec).upper("Chauff.") shouldBe 60
  }

  it should "use actual daily rate when it exceeds minDailyChange" in {
    // ConsommationEnergie Total: daily rate = 325/31 ≈ 10.48, which exceeds minDailyChange = 1.0
    val bounds = ValueBounds.compute(reading2, Some(reading1), newDate, 2.0, 1.0)
    bounds(PhotoType.ConsommationEnergie).upper("Total") shouldBe 7955
  }

  it should "return empty upper bounds when newDate is before or equal to lastRow date" in {
    val sameDate = reading2.date
    val bounds = ValueBounds.compute(reading2, Some(reading1), sameDate, 2.0, 1.0)
    bounds(PhotoType.ConsommationEnergie).upper shouldBe empty
  }

  "ValueBounds.checkReading" should "return no warnings when values are within bounds" in {
    val bounds = ValueBounds.compute(reading2, Some(reading1), newDate, 2.0, 1.0)
    // A reading with values between lower and upper bounds
    val goodReading = HeatPumpReading(
      date = newDate,
      consommationTotale = 7500,
      chauffageAppointChauff = 0,
      chauffageAppointECS = 5,
      compresseurTotal = 7500,
      compresseurChauff = 4500,
      compresseurECS = 2600,
      compresseurRefroid = 350,
      energieFournieTotale = 26000,
      energieFournieChauffage = 16000,
      energieFournieECS = 7200,
      energieFournieRefroid = 2600
    )

    val warnings = ValueBounds.checkReading(goodReading, bounds)
    warnings shouldBe empty
  }

  it should "warn when a value is below the lower bound" in {
    val bounds = ValueBounds.compute(reading2, None, newDate, 2.0, 1.0)
    val badReading = reading2.copy(date = newDate, consommationTotale = 7000) // below 7325

    val warnings = ValueBounds.checkReading(badReading, bounds)
    warnings should contain key "consommationTotale"
    warnings("consommationTotale") should include("below lower bound")
  }

  it should "warn when a value is above the upper bound" in {
    val bounds = ValueBounds.compute(reading2, Some(reading1), newDate, 2.0, 1.0)
    val badReading = reading2.copy(date = newDate, consommationTotale = 99999) // way above upper

    val warnings = ValueBounds.checkReading(badReading, bounds)
    warnings should contain key "consommationTotale"
    warnings("consommationTotale") should include("above upper bound")
  }

  "ValueBounds.formatForPrompt" should "produce a non-empty prompt section" in {
    val bounds = ValueBounds.compute(reading2, Some(reading1), newDate, 2.0, 1.0)
    val prompt = ValueBounds.formatForPrompt(bounds)

    prompt should include("Consommation d'énergie")
    prompt should include("Compresseur")
    prompt should include("between")
  }

  it should "use 'at least' when only lower bounds are available" in {
    val bounds = ValueBounds.compute(reading2, None, newDate, 2.0, 1.0)
    val prompt = ValueBounds.formatForPrompt(bounds)

    prompt should include("at least")
    prompt should not include "between"
  }
}
