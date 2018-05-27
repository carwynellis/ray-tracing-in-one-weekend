package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

/**
  * TODO - factor out image writing to clean things up here.
  *      - proper error handling of file write errors
  */
object Render extends App {

  val nx = 800
  val ny = 600

  /**
    * Perform a linear blend from blue to white.
    * @param r
    * @return
    */
  def color(r: Ray): Vec3 = {
    val unitDirection = r.direction.unitVector
    val t = 0.5 * ( unitDirection.y + 1)
    ((1.0 - t) * Vec3(1, 1, 1)) + (t * Vec3(0.5, 0.7, 100))
  }

  val fileName = "image.ppm"
  val writer = new PrintWriter(new File(fileName))

  println(s"Rendering image to $fileName")

  // Write PPM header
  writer.write(
    s"""P3
      |$nx
      |$ny
      |255
      |""".stripMargin)

  val lowerLeftCorner = Vec3(-2, -1, -1)
  val horizontal      = Vec3(4, 0, 0)
  val vertical        = Vec3(0, 2, 0)
  val origin          = Vec3(0, 0, 0)

  // Write PPM data
  (ny-1 to 0 by -1) foreach { j =>
    (0 until nx) foreach { i =>
      val u = i.toDouble / nx
      val v = j.toDouble / ny
      val ray = Ray(
        origin = origin,
        direction = lowerLeftCorner + (u * horizontal) + (v * vertical)
      )
      val c = color(ray)

      val ir = (255.99 * c.x).toInt
      val ig = (255.99 * c.y).toInt
      val ib = (255.99 * c.z).toInt

      writer.write(s"$ir $ig $ib\n")
    }
  }

  writer.close()

  println("Finished")
}
