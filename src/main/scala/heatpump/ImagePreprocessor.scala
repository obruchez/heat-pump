package heatpump

import java.nio.file.{Files, Path}
import scala.sys.process._

object ImagePreprocessor {

  /**
   * Resizes an image using ImageMagick if it exceeds maxWidth.
   * Returns the path to the (possibly resized) image.
   * If maxWidth is 0, returns the original path unchanged.
   */
  def resizeIfNeeded(imagePath: Path, maxWidth: Int): Either[String, Path] = {
    if (maxWidth <= 0) return Right(imagePath)

    try {
      // Create a temporary file for the resized image
      val fileName = imagePath.getFileName.toString
      val dotIdx = fileName.lastIndexOf('.')
      val ext = if (dotIdx > 0) fileName.substring(dotIdx) else ".jpg"
      val resizedPath = Files.createTempFile("heatpump_", ext)

      // Use ImageMagick to resize (only shrinks, never enlarges due to >)
      val cmd = Seq("magick", "convert", imagePath.toString, "-resize", s"${maxWidth}x>", resizedPath.toString)
      val exitCode = cmd.!

      if (exitCode == 0) {
        Right(resizedPath)
      } else {
        // Clean up temp file on failure, fall back to original
        Files.deleteIfExists(resizedPath)
        println(s"  Warning: ImageMagick resize failed for ${imagePath.getFileName}, using original")
        Right(imagePath)
      }
    } catch {
      case _: Exception =>
        println(s"  Warning: ImageMagick not available, using original image for ${imagePath.getFileName}")
        Right(imagePath)
    }
  }

  /** Cleans up temporary resized files. */
  def cleanup(tempFiles: List[Path], originalFiles: List[Path]): Unit = {
    tempFiles.zip(originalFiles).foreach { case (temp, original) =>
      if (temp != original) {
        try { Files.deleteIfExists(temp) } catch { case _: Exception => }
      }
    }
  }
}
