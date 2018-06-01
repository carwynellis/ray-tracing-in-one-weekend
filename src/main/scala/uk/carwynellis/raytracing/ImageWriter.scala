package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

/**
  * Simple image writer that outputs PPM files.
  *
  * TODO - make sure that the writer is closed on error.
  *
  * @param width
  * @param height
  * @param filename
  */
class ImageWriter(width: Int, height: Int, filename: String) {

  private lazy val writer = {
    // Initialize the PrintWriter and output the PPM header.
    val w = new PrintWriter(new File(filename))

    w.write(
      s"""P3
         |$width
         |$height
         |255
         |""".stripMargin)

    w
  }

  def writePixel(r: Int, g: Int, b: Int): Unit = writer.write(s"$r $g $b\n")

  def close() = writer.close()
}

object ImageWriter {
  def apply(width: Int, height: Int, filename: String) = new ImageWriter(width, height, filename)
}
