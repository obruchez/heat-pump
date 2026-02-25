package heatpump

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.parser._
import io.circe.generic.semiauto._
import io.circe._

class ClaudeClientSpec extends AnyFlatSpec with Matchers {

  // Test the JSON parsing logic that ClaudeClient uses internally
  private case class InnerResponse(
      `type`: String,
      values: Map[String, Option[Int]],
      confident: Boolean
  )
  private implicit val decodeInnerResponse: Decoder[InnerResponse] = deriveDecoder[InnerResponse]

  "Claude response JSON parsing" should "parse a well-formed response" in {
    val json = """{
      "type": "Compresseur",
      "values": {
        "Total": 7320,
        "Chauff.": 4417,
        "Eau chaude sanitaire": 2559,
        "Refroid.": 334
      },
      "confident": true
    }"""

    val result = decode[InnerResponse](json)
    result shouldBe a[Right[_, _]]

    val response = result.toOption.get
    response.`type` shouldBe "Compresseur"
    response.values("Total") shouldBe Some(7320)
    response.values("Chauff.") shouldBe Some(4417)
    response.values("Eau chaude sanitaire") shouldBe Some(2559)
    response.values("Refroid.") shouldBe Some(334)
    response.confident shouldBe true
  }

  it should "handle null values for unreadable fields" in {
    val json = """{
      "type": "Consommation d'énergie",
      "values": {
        "Total": null
      },
      "confident": false
    }"""

    val result = decode[InnerResponse](json)
    result shouldBe a[Right[_, _]]

    val response = result.toOption.get
    response.`type` shouldBe "Consommation d'énergie"
    response.values("Total") shouldBe None
    response.confident shouldBe false
  }

  it should "parse Photo 1 (Consommation d'énergie) response" in {
    val json = """{
      "type": "Consommation d'énergie",
      "values": {"Total": 7325},
      "confident": true
    }"""

    val result = decode[InnerResponse](json)
    result shouldBe a[Right[_, _]]
    result.toOption.get.values("Total") shouldBe Some(7325)
  }

  it should "parse Photo 2 (Chauffage appoint élec.) response" in {
    val json = """{
      "type": "Chauffage appoint élec.",
      "values": {"Total": 5, "Chauff.": 0, "Eau chaude sanitaire": 5},
      "confident": true
    }"""

    val result = decode[InnerResponse](json)
    result shouldBe a[Right[_, _]]
    val values = result.toOption.get.values
    values("Total") shouldBe Some(5)
    values("Chauff.") shouldBe Some(0)
    values("Eau chaude sanitaire") shouldBe Some(5)
  }

  it should "parse Photo 4 (Energie fournie) response" in {
    val json = """{
      "type": "Energie fournie",
      "values": {"Total": 25280, "Chauffage": 15738, "ECS": 7048, "Refroid.": 2496},
      "confident": true
    }"""

    val result = decode[InnerResponse](json)
    result shouldBe a[Right[_, _]]
    val values = result.toOption.get.values
    values("Total") shouldBe Some(25280)
    values("Chauffage") shouldBe Some(15738)
    values("ECS") shouldBe Some(7048)
    values("Refroid.") shouldBe Some(2496)
  }

  "Markdown fence stripping" should "handle json code fences" in {
    val raw = "```json\n{\"type\":\"Compresseur\",\"values\":{\"Total\":100},\"confident\":true}\n```"
    val cleaned = raw.trim.stripPrefix("```json").stripPrefix("```").stripSuffix("```").trim
    val result = decode[InnerResponse](cleaned)
    result shouldBe a[Right[_, _]]
    result.toOption.get.`type` shouldBe "Compresseur"
  }

  it should "handle plain code fences" in {
    val raw = "```\n{\"type\":\"Compresseur\",\"values\":{\"Total\":100},\"confident\":true}\n```"
    val cleaned = raw.trim.stripPrefix("```json").stripPrefix("```").stripSuffix("```").trim
    val result = decode[InnerResponse](cleaned)
    result shouldBe a[Right[_, _]]
  }

  it should "handle no fences" in {
    val raw = "{\"type\":\"Compresseur\",\"values\":{\"Total\":100},\"confident\":true}"
    val cleaned = raw.trim.stripPrefix("```json").stripPrefix("```").stripSuffix("```").trim
    val result = decode[InnerResponse](cleaned)
    result shouldBe a[Right[_, _]]
  }
}
