package heatpump

import scala.io.StdIn

object UserInput {

  /**
   * Prompts the user to manually enter a value that could not be read from a photo.
   * Returns the integer value entered by the user.
   */
  def promptForValue(photoType: PhotoType, label: String): Int = {
    println(s"\n⚠ Could not confidently read value for '${photoType.label}' > '$label'")
    print(s"  Please enter the value manually: ")
    var result: Option[Int] = None
    while (result.isEmpty) {
      val input = StdIn.readLine()
      try {
        result = Some(input.trim.replaceAll("[,\\s]", "").toInt)
      } catch {
        case _: NumberFormatException =>
          print(s"  Invalid number. Please enter an integer value: ")
      }
    }
    result.get
  }

  /**
   * Resolves all null/uncertain values in a ClaudePhotoResponse by prompting the user.
   * Returns an ExtractedPhoto with all values filled in.
   */
  def resolveUncertainValues(response: ClaudePhotoResponse, photoType: PhotoType): ExtractedPhoto = {
    val resolvedValues = response.values.map { case (label, valueOpt) =>
      val value = valueOpt.getOrElse(promptForValue(photoType, label))
      label -> value
    }
    ExtractedPhoto(photoType, resolvedValues)
  }

  /** Asks the user a yes/no question, returns true for yes. */
  def confirm(message: String): Boolean = {
    print(s"$message (y/n): ")
    val input = StdIn.readLine()
    input != null && input.trim.toLowerCase.startsWith("y")
  }
}
