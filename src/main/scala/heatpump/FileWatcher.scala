package heatpump

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object FileWatcher {

  private val ImageExtensions = Set(".jpg", ".jpeg", ".png")

  /** Returns true if the path is an image file (by extension). */
  private def isImageFile(path: Path): Boolean = {
    val name = path.getFileName.toString.toLowerCase
    ImageExtensions.exists(name.endsWith)
  }

  /** Lists current image files in the directory. */
  private def listImageFiles(directory: Path): List[Path] = {
    if (!Files.isDirectory(directory)) return Nil
    Files.list(directory).iterator().asScala
      .filter(p => Files.isRegularFile(p) && isImageFile(p))
      .toList
      .sortBy(_.getFileName.toString)
  }

  /**
   * Polls the directory until exactly 4 image files are present.
   * Returns the list of 4 image file paths.
   */
  def waitForPhotos(directory: Path, pollIntervalSeconds: Int): List[Path] = {
    println(s"Watching directory: $directory")
    println("Waiting for 4 image files (.jpg, .jpeg, .png)...")

    var images = listImageFiles(directory)
    while (images.size < 4) {
      val count = images.size
      if (count > 0) {
        println(s"  Found $count image(s) so far, waiting for ${4 - count} more...")
      }
      Thread.sleep(pollIntervalSeconds * 1000L)
      images = listImageFiles(directory)
    }

    if (images.size > 4) {
      println(s"Warning: Found ${images.size} images, expected 4. Using the first 4.")
      images = images.take(4)
    }

    println(s"Found 4 images:")
    images.foreach(p => println(s"  - ${p.getFileName}"))
    images
  }

  /**
   * Moves processed files to the processed directory.
   * Creates the directory if it doesn't exist.
   * Renames files to avoid collisions.
   */
  def moveToProcessed(files: List[Path], watchDir: Path, processedDirName: String): Either[String, Unit] =
    try {
      val processedDir = watchDir.resolve(processedDirName)
      if (!Files.exists(processedDir)) {
        Files.createDirectories(processedDir)
      }

      files.foreach { file =>
        var target = processedDir.resolve(file.getFileName)
        // Handle name collisions
        var counter = 1
        while (Files.exists(target)) {
          val name = file.getFileName.toString
          val dotIdx = name.lastIndexOf('.')
          val (base, ext) = if (dotIdx > 0) (name.substring(0, dotIdx), name.substring(dotIdx)) else (name, "")
          target = processedDir.resolve(s"${base}_$counter$ext")
          counter += 1
        }
        Files.move(file, target)
        println(s"  Moved ${file.getFileName} -> $processedDirName/${target.getFileName}")
      }

      Right(())
    } catch {
      case e: Exception => Left(s"Failed to move files: ${e.getMessage}")
    }
}
