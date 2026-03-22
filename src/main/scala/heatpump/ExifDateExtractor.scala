package heatpump

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.ExifSubIFDDirectory
import java.nio.file.Path
import java.time.LocalDate
import java.time.ZoneId

object ExifDateExtractor {

  /** Extracts the date (as LocalDate) from the EXIF metadata of an image file. */
  def extractDate(imagePath: Path): Either[String, LocalDate] =
    try {
      val metadata = ImageMetadataReader.readMetadata(imagePath.toFile)
      val exifDir = metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory])
      if (exifDir == null) {
        Left(s"No EXIF data found in ${imagePath.getFileName}")
      } else {
        val date = exifDir.getDateOriginal
        if (date == null) {
          Left(s"No date found in EXIF data of ${imagePath.getFileName}")
        } else {
          Right(date.toInstant.atZone(ZoneId.systemDefault()).toLocalDate)
        }
      }
    } catch {
      case e: Exception => Left(s"Failed to read EXIF data from ${imagePath.getFileName}: ${e.getMessage}")
    }

  /**
   * Extracts dates from all photos and returns the common date if they all match.
   * Returns an error if dates differ or any date cannot be extracted.
   */
  def findCommonDate(photos: List[Path]): Either[String, LocalDate] = {
    val results = photos.map(p => (p, extractDate(p)))

    val errors = results.collect { case (_, Left(err)) => err }
    if (errors.nonEmpty) {
      return Left(s"Failed to extract date from photos:\n  ${errors.mkString("\n  ")}")
    }

    val dates = results.collect { case (_, Right(d)) => d }
    val distinctDates = dates.distinct
    if (distinctDates.size == 1) {
      Right(distinctDates.head)
    } else {
      val details = results.collect { case (p, Right(d)) => s"${p.getFileName}: $d" }
      Left(s"Photos have different dates:\n  ${details.mkString("\n  ")}")
    }
  }
}
