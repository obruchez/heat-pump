package heatpump

import com.typesafe.config.ConfigFactory
import java.nio.file.{Path, Paths}

case class AppConfig(
    watchDirectory: Path,
    processedDirectory: String,
    claudeApiKey: String,
    claudeModel: String,
    spreadsheetId: String,
    credentialsFile: Path,
    maxImageWidth: Int,
    pollIntervalSeconds: Int
)

object Config {
  def load(): Either[String, AppConfig] =
    try {
      val config = ConfigFactory.load().getConfig("heat-pump")
      Right(
        AppConfig(
          watchDirectory = Paths.get(config.getString("watch-directory")),
          processedDirectory = config.getString("processed-directory"),
          claudeApiKey = config.getString("claude-api-key"),
          claudeModel = config.getString("claude-model"),
          spreadsheetId = config.getString("spreadsheet-id"),
          credentialsFile = Paths.get(config.getString("credentials-file")),
          maxImageWidth = config.getInt("max-image-width"),
          pollIntervalSeconds = config.getInt("poll-interval-seconds")
        )
      )
    } catch {
      case e: Exception => Left(s"Failed to load configuration: ${e.getMessage}")
    }
}
