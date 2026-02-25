package heatpump

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.sheets.v4.{Sheets, SheetsScopes}
import com.google.api.services.sheets.v4.model._
import com.google.auth.http.HttpCredentialsAdapter
import com.google.auth.oauth2.ServiceAccountCredentials
import java.io.FileInputStream
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._

object GoogleSheetsClient {

  private val SheetName = "Sheet1"

  /** Result of duplicate checking. */
  sealed trait DuplicateCheckResult
  case object NoDuplicate extends DuplicateCheckResult
  case object ExactDuplicate extends DuplicateCheckResult
  case class DifferentValues(existingRow: List[String]) extends DuplicateCheckResult

  /** Creates an authenticated Sheets service. */
  def buildService(config: AppConfig): Either[String, Sheets] =
    try {
      val credentials = ServiceAccountCredentials
        .fromStream(new FileInputStream(config.credentialsFile.toFile))
        .createScoped(java.util.Collections.singletonList(SheetsScopes.SPREADSHEETS))

      val httpTransport = GoogleNetHttpTransport.newTrustedTransport()
      val jsonFactory = GsonFactory.getDefaultInstance

      Right(
        new Sheets.Builder(httpTransport, jsonFactory, new HttpCredentialsAdapter(credentials))
          .setApplicationName("heat-pump")
          .build()
      )
    } catch {
      case e: Exception => Left(s"Failed to initialize Google Sheets service: ${e.getMessage}")
    }

  /** Checks if a row with today's date already exists. */
  def checkDuplicate(
      service: Sheets,
      spreadsheetId: String,
      reading: HeatPumpReading
  ): Either[String, DuplicateCheckResult] =
    try {
      val dateStr = reading.date.format(DateTimeFormatter.ISO_LOCAL_DATE)
      val range = s"$SheetName!A:L"
      val response = service.spreadsheets().values().get(spreadsheetId, range).execute()
      val rows = Option(response.getValues).map(_.asScala.toList).getOrElse(Nil)

      // Skip header row (index 0), find rows matching the date
      val matchingRows = rows.drop(1).filter { row =>
        val cells = row.asScala.toList.map(_.toString)
        cells.nonEmpty && cells.head == dateStr
      }

      matchingRows.headOption match {
        case None => Right(NoDuplicate)
        case Some(existingRow) =>
          val existingCells = existingRow.asScala.toList.map(_.toString)
          val newCells = reading.toRow
          if (existingCells == newCells) Right(ExactDuplicate)
          else Right(DifferentValues(existingCells))
      }
    } catch {
      case e: Exception => Left(s"Failed to check for duplicates: ${e.getMessage}")
    }

  /**
   * Inserts a new row at position 2 (right after the header) and writes the values.
   * Uses InsertDimensionRequest to push existing rows down, then updates A2:L2.
   */
  def insertRow(
      service: Sheets,
      spreadsheetId: String,
      reading: HeatPumpReading
  ): Either[String, Unit] =
    try {
      // Step 1: Get the sheet ID (needed for InsertDimensionRequest)
      val sheetId = getSheetId(service, spreadsheetId)

      // Step 2: Insert an empty row at index 1 (row 2 in 1-based)
      val insertRequest = new InsertDimensionRequest()
        .setRange(
          new DimensionRange()
            .setSheetId(sheetId)
            .setDimension("ROWS")
            .setStartIndex(1)  // 0-based, so row 2
            .setEndIndex(2)
        )
        .setInheritFromBefore(false)

      val batchRequest = new BatchUpdateSpreadsheetRequest()
        .setRequests(java.util.Collections.singletonList(
          new Request().setInsertDimension(insertRequest)
        ))

      service.spreadsheets().batchUpdate(spreadsheetId, batchRequest).execute()

      // Step 3: Write values to A2:L2
      val values = reading.toRow.map(v => v: AnyRef).asJava
      val body = new ValueRange()
        .setValues(java.util.Collections.singletonList(values))

      service.spreadsheets().values()
        .update(spreadsheetId, s"$SheetName!A2:L2", body)
        .setValueInputOption("RAW")
        .execute()

      Right(())
    } catch {
      case e: Exception => Left(s"Failed to insert row: ${e.getMessage}")
    }

  private def getSheetId(service: Sheets, spreadsheetId: String): Int = {
    val spreadsheet = service.spreadsheets().get(spreadsheetId).execute()
    spreadsheet.getSheets.get(0).getProperties.getSheetId
  }
}
