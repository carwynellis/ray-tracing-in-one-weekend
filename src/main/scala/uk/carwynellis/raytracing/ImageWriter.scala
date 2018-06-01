package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

/**
  * Simple image writer that outputs PPM files.
  *
  * Note that this writer doesn't take care of error handling which is probably good enough here for now.
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

  def writePixel(p: Pixel): Unit = writer.write(s"${p.r} ${p.g} ${p.b}\n")

  def close() = writer.close()
}

object ImageWriter {
  def apply(width: Int, height: Int, filename: String) = new ImageWriter(width, height, filename)
}
