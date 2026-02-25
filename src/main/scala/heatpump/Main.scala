package heatpump

import java.time.LocalDate

object Main {

  def main(args: Array[String]): Unit = {
    println("=== Heat Pump Photo Processor ===\n")

    // Step 1: Load configuration
    val config = Config.load() match {
      case Right(c) => c
      case Left(err) =>
        System.err.println(s"Configuration error: $err")
        sys.exit(1)
    }

    // Step 2: Wait for 4 photos
    val photos = FileWatcher.waitForPhotos(config.watchDirectory, config.pollIntervalSeconds)

    // Step 3: Preprocess (resize) images
    println("\nPreprocessing images...")
    val resizedPhotos = photos.map { photo =>
      ImagePreprocessor.resizeIfNeeded(photo, config.maxImageWidth) match {
        case Right(resized) => resized
        case Left(err) =>
          println(s"  Warning: $err")
          photo
      }
    }

    // Step 4: Send each photo to Claude API
    println("\nAnalyzing photos with Claude API...")
    val extractedPhotos = resizedPhotos.zip(photos).map { case (resizedPath, originalPath) =>
      println(s"  Processing ${originalPath.getFileName}...")
      ClaudeClient.analyzePhoto(resizedPath, config) match {
        case Right(response) =>
          val photoType = PhotoType.fromString(response.photoType) match {
            case Some(pt) =>
              println(s"    Identified as: ${pt.label}")
              pt
            case None =>
              System.err.println(s"    Error: Unknown photo type '${response.photoType}'")
              sys.exit(1)
          }

          // Resolve uncertain values via user input
          if (!response.confident || response.values.values.exists(_.isEmpty)) {
            println(s"    Some values are uncertain, prompting for confirmation...")
            UserInput.resolveUncertainValues(response, photoType)
          } else {
            val values = response.values.collect { case (k, Some(v)) => k -> v }
            ExtractedPhoto(photoType, values)
          }

        case Left(err) =>
          System.err.println(s"    Error analyzing ${originalPath.getFileName}: $err")
          sys.exit(1)
      }
    }

    // Cleanup temp files
    ImagePreprocessor.cleanup(resizedPhotos, photos)

    // Step 5: Assemble reading
    val today = LocalDate.now()
    println(s"\nAssembling reading for date: $today")
    val reading = HeatPumpReading.fromPhotos(extractedPhotos, today) match {
      case Right(r) => r
      case Left(err) =>
        System.err.println(s"Error assembling reading: $err")
        sys.exit(1)
    }

    // Display summary
    println("\nExtracted values:")
    println(s"  Date:                              ${reading.date}")
    println(s"  B - Consommation totale:           ${reading.consommationTotale}")
    println(s"  C - Chauffage appoint (chauffage):  ${reading.chauffageAppointChauff}")
    println(s"  D - Chauffage appoint (ECS):        ${reading.chauffageAppointECS}")
    println(s"  E - Compresseur total:              ${reading.compresseurTotal}")
    println(s"  F - Compresseur chauffage:           ${reading.compresseurChauff}")
    println(s"  G - Compresseur ECS:                 ${reading.compresseurECS}")
    println(s"  H - Compresseur refroid.:            ${reading.compresseurRefroid}")
    println(s"  I - Energie fournie totale:          ${reading.energieFournieTotale}")
    println(s"  J - Energie fournie chauffage:       ${reading.energieFournieChauffage}")
    println(s"  K - Energie fournie ECS:             ${reading.energieFournieECS}")
    println(s"  L - Energie fournie refroid.:        ${reading.energieFournieRefroid}")

    if (!UserInput.confirm("\nInsert these values into the Google Sheet?")) {
      println("Aborted by user.")
      sys.exit(0)
    }

    // Step 6: Google Sheets integration
    println("\nConnecting to Google Sheets...")
    val sheetsService = GoogleSheetsClient.buildService(config) match {
      case Right(s) => s
      case Left(err) =>
        System.err.println(s"Google Sheets error: $err")
        sys.exit(1)
    }

    // Check for duplicates
    GoogleSheetsClient.checkDuplicate(sheetsService, config.spreadsheetId, reading) match {
      case Right(GoogleSheetsClient.ExactDuplicate) =>
        println("A row with today's date and identical values already exists. No insertion needed.")

      case Right(GoogleSheetsClient.DifferentValues(existing)) =>
        println(s"Warning: A row with today's date exists but has different values:")
        println(s"  Existing: ${existing.mkString(", ")}")
        println(s"  New:      ${reading.toRow.mkString(", ")}")
        if (UserInput.confirm("Insert a new row with the new values anyway?")) {
          insertAndReport(sheetsService, config.spreadsheetId, reading)
        } else {
          println("Skipped insertion.")
        }

      case Right(GoogleSheetsClient.NoDuplicate) =>
        insertAndReport(sheetsService, config.spreadsheetId, reading)

      case Left(err) =>
        System.err.println(s"Duplicate check failed: $err")
        System.err.println("Proceeding with insertion anyway...")
        insertAndReport(sheetsService, config.spreadsheetId, reading)
    }

    // Step 7: Move processed files
    println("\nMoving processed files...")
    FileWatcher.moveToProcessed(photos, config.watchDirectory, config.processedDirectory) match {
      case Right(()) => ()
      case Left(err) => System.err.println(s"Warning: $err")
    }

    println("\nDone!")
  }

  private def insertAndReport(
      service: com.google.api.services.sheets.v4.Sheets,
      spreadsheetId: String,
      reading: HeatPumpReading
  ): Unit = {
    GoogleSheetsClient.insertRow(service, spreadsheetId, reading) match {
      case Right(()) =>
        println("Row inserted successfully into the Google Sheet.")
      case Left(err) =>
        System.err.println(s"Failed to insert row: $err")
        sys.exit(1)
    }
  }
}
