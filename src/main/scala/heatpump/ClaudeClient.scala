package heatpump

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.util.Base64

object ClaudeClient {

  private val ApiUrl = "https://api.anthropic.com/v1/messages"
  private val ApiVersion = "2023-06-01"

  private val httpClient = HttpClient.newHttpClient()

  /** The structured prompt sent to Claude for each photo. */
  private val systemPrompt =
    """You are analyzing a photo of a heat pump LCD screen. The text is in French.
      |The screen shows energy values in kWh (the unit may appear as "kW" due to space constraints, but it is always kWh).
      |
      |Identify which of the 4 screen types this photo shows:
      |1. "Consommation d'énergie" - Shows: Total
      |2. "Chauffage appoint élec." - Shows: Total, Chauff., Eau chaude sanitaire
      |3. "Compresseur" - Shows: Total, Chauff., Eau chaude sanitaire, Refroid.
      |4. "Energie fournie" - Shows: Total, Chauffage, ECS, Refroid. (labels may vary: "Energie fournie Total", "Energie fournie Chauffage", "Energie fournie ECS", "Energie de ref. fournie")
      |
      |Return a JSON object with:
      |- "type": the screen title (one of the 4 types above, use the exact French label)
      |- "values": an object mapping each label to its integer value (without units). Use null if a value is unreadable.
      |  For screen type 1, use key "Total".
      |  For screen type 2, use keys "Total", "Chauff.", "Eau chaude sanitaire".
      |  For screen type 3, use keys "Total", "Chauff.", "Eau chaude sanitaire", "Refroid.".
      |  For screen type 4, use keys "Total", "Chauffage", "ECS", "Refroid.".
      |- "confident": true if you are confident in all values, false if any value is uncertain.
      |
      |Return ONLY the JSON object, no other text.""".stripMargin

  /** Case class for parsing Claude's inner JSON response. */
  private case class InnerResponse(
      `type`: String,
      values: Map[String, Option[Int]],
      confident: Boolean
  )
  private implicit val decodeInnerResponse: Decoder[InnerResponse] = deriveDecoder[InnerResponse]

  /** Sends a photo to Claude and extracts structured data. */
  def analyzePhoto(imagePath: Path, config: AppConfig): Either[String, ClaudePhotoResponse] = {
    for {
      base64Data <- encodeImage(imagePath)
      mediaType  <- detectMediaType(imagePath)
      response   <- sendRequest(base64Data, mediaType, config)
      parsed     <- parseResponse(response)
    } yield parsed
  }

  private def encodeImage(path: Path): Either[String, String] =
    try {
      val bytes = Files.readAllBytes(path)
      Right(Base64.getEncoder.encodeToString(bytes))
    } catch {
      case e: Exception => Left(s"Failed to read image ${path.getFileName}: ${e.getMessage}")
    }

  private def detectMediaType(path: Path): Either[String, String] = {
    val name = path.getFileName.toString.toLowerCase
    if (name.endsWith(".jpg") || name.endsWith(".jpeg")) Right("image/jpeg")
    else if (name.endsWith(".png")) Right("image/png")
    else Left(s"Unsupported image format: $name (expected .jpg, .jpeg, or .png)")
  }

  private def sendRequest(base64Data: String, mediaType: String, config: AppConfig): Either[String, String] = {
    val requestBody = Json.obj(
      "model" -> config.claudeModel.asJson,
      "max_tokens" -> 1024.asJson,
      "messages" -> Json.arr(
        Json.obj(
          "role" -> "user".asJson,
          "content" -> Json.arr(
            Json.obj(
              "type" -> "image".asJson,
              "source" -> Json.obj(
                "type" -> "base64".asJson,
                "media_type" -> mediaType.asJson,
                "data" -> base64Data.asJson
              )
            ),
            Json.obj(
              "type" -> "text".asJson,
              "text" -> systemPrompt.asJson
            )
          )
        )
      )
    )

    try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create(ApiUrl))
        .header("Content-Type", "application/json")
        .header("x-api-key", config.claudeApiKey)
        .header("anthropic-version", ApiVersion)
        .POST(HttpRequest.BodyPublishers.ofString(requestBody.noSpaces))
        .build()

      val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())

      if (response.statusCode() == 200) {
        Right(response.body())
      } else {
        Left(s"Claude API returned status ${response.statusCode()}: ${response.body()}")
      }
    } catch {
      case e: Exception => Left(s"Claude API request failed: ${e.getMessage}")
    }
  }

  private def parseResponse(responseBody: String): Either[String, ClaudePhotoResponse] = {
    for {
      json <- parse(responseBody).left.map(e => s"Failed to parse Claude response JSON: ${e.getMessage}")
      text <- extractTextContent(json)
      inner <- parseInnerJson(text)
    } yield ClaudePhotoResponse(
      photoType = inner.`type`,
      values = inner.values,
      confident = inner.confident
    )
  }

  /** Extracts the text from content[0].text in the Claude API response. */
  private def extractTextContent(json: Json): Either[String, String] = {
    val cursor = json.hcursor
    cursor.downField("content").downArray.downField("text").as[String]
      .left.map(_ => {
        // Try to extract error message
        val errorMsg = cursor.downField("error").downField("message").as[String].getOrElse("unknown error")
        s"Failed to extract text from Claude response: $errorMsg"
      })
  }

  /** Parses the inner JSON (Claude's actual answer) which may be wrapped in markdown code fences. */
  private def parseInnerJson(text: String): Either[String, InnerResponse] = {
    // Strip potential markdown code fences
    val cleaned = text.trim
      .stripPrefix("```json").stripPrefix("```")
      .stripSuffix("```")
      .trim

    decode[InnerResponse](cleaned)
      .left.map(e => s"Failed to parse Claude's inner JSON: ${e.getMessage}\nRaw text: $text")
  }
}
