package heatpump

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

    // Step 3: Extract common date from photo EXIF data
    val photoDate = ExifDateExtractor.findCommonDate(photos) match {
      case Right(d) => d
      case Left(err) =>
        System.err.println(s"Date extraction error: $err")
        sys.exit(1)
    }
    println(s"\nPhoto date: $photoDate")

    // Step 4: Connect to Google Sheets and read historical data for bounds
    println("\nConnecting to Google Sheets...")
    val sheetsService = GoogleSheetsClient.buildService(config) match {
      case Right(s) => s
      case Left(err) =>
        System.err.println(s"Google Sheets error: $err")
        sys.exit(1)
    }

    val bounds: Map[PhotoType, PhotoBounds] = GoogleSheetsClient.readLastRows(sheetsService, config.spreadsheetId) match {
      case Right((Some(lastRow), secondToLastRow)) =>
        val b = ValueBounds.compute(lastRow, secondToLastRow, photoDate, config.boundsFactor, config.boundsMinDailyChange)
        println(s"  Computed bounds from last row (${lastRow.date})" +
          secondToLastRow.map(r => s" and previous row (${r.date})").getOrElse("") +
          s" with factor ${config.boundsFactor}")
        println()
        ValueBounds.printBounds(b)
        b
      case Right((None, _)) =>
        println("  No historical data found, skipping bounds.")
        Map.empty
      case Left(err) =>
        println(s"  Warning: Could not read historical data: $err")
        println("  Proceeding without bounds.")
        Map.empty
    }

    // Step 5: Preprocess (resize) images
    println("\nPreprocessing images...")
    val resizedPhotos = photos.map { photo =>
      ImagePreprocessor.resizeIfNeeded(photo, config.maxImageWidth) match {
        case Right(resized) => resized
        case Left(err) =>
          println(s"  Warning: $err")
          photo
      }
    }

    // Step 6: Send each photo to Claude API
    println("\nAnalyzing photos with Claude API...")
    val extractedPhotos = resizedPhotos.zip(photos).map { case (resizedPath, originalPath) =>
      println(s"  Processing ${originalPath.getFileName}...")
      ClaudeClient.analyzePhoto(resizedPath, config, bounds) match {
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

    // Step 7: Assemble reading
    println(s"\nAssembling reading for date: $photoDate")
    val reading = HeatPumpReading.fromPhotos(extractedPhotos, photoDate) match {
      case Right(r) => r
      case Left(err) =>
        System.err.println(s"Error assembling reading: $err")
        sys.exit(1)
    }

    // Step 8: Check bounds and display summary
    val warnings = if (bounds.nonEmpty) ValueBounds.checkReading(reading, bounds) else Map.empty[String, String]

    def warn(fieldId: String): String =
      warnings.get(fieldId).map(w => s"  ⚠ $w").getOrElse("")

    println("\nExtracted values:")
    println(s"  Date:                              ${reading.date}")
    println(s"  B - Consommation totale:           ${reading.consommationTotale}${warn("consommationTotale")}")
    println(s"  C - Chauffage appoint (chauffage):  ${reading.chauffageAppointChauff}${warn("chauffageAppointChauff")}")
    println(s"  D - Chauffage appoint (ECS):        ${reading.chauffageAppointECS}${warn("chauffageAppointECS")}")
    println(s"  E - Compresseur total:              ${reading.compresseurTotal}${warn("compresseurTotal")}")
    println(s"  F - Compresseur chauffage:           ${reading.compresseurChauff}${warn("compresseurChauff")}")
    println(s"  G - Compresseur ECS:                 ${reading.compresseurECS}${warn("compresseurECS")}")
    println(s"  H - Compresseur refroid.:            ${reading.compresseurRefroid}${warn("compresseurRefroid")}")
    println(s"  I - Energie fournie totale:          ${reading.energieFournieTotale}${warn("energieFournieTotale")}")
    println(s"  J - Energie fournie chauffage:       ${reading.energieFournieChauffage}${warn("energieFournieChauffage")}")
    println(s"  K - Energie fournie ECS:             ${reading.energieFournieECS}${warn("energieFournieECS")}")
    println(s"  L - Energie fournie refroid.:        ${reading.energieFournieRefroid}${warn("energieFournieRefroid")}")

    if (!UserInput.confirm("\nInsert these values into the Google Sheet?")) {
      println("Aborted by user.")
      sys.exit(0)
    }

    // Step 9: Check for duplicates and insert
    GoogleSheetsClient.checkDuplicate(sheetsService, config.spreadsheetId, reading) match {
      case Right(GoogleSheetsClient.ExactDuplicate) =>
        println("A row with this date and identical values already exists. No insertion needed.")

      case Right(GoogleSheetsClient.DifferentValues(existing)) =>
        println(s"Warning: A row with this date exists but has different values:")
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

    // Step 10: Move processed files
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
