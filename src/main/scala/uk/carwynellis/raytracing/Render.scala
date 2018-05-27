package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

/**
  * TODO - factor out image writing to clean things up here.
  *      - proper error handling of file write errors
  */
object Render extends App {

  val nx = 200
  val ny = 100

  /**
    * Compute the color for a given ray.
    *
    * @param r
    * @return
    */
  def color(r: Ray): Vec3 = {
    if (hitSphere(Vec3(0, 0, -1), 0.5, r)) {
      // Ray has intersected sphere so we return red for the current pixel.
      Vec3(1, 0, 0)
    }
    else {
      val unitDirection = r.direction.unitVector
      val t = 0.5 * (unitDirection.y + 1)
      ((1.0 - t) * Vec3(1, 1, 1)) + (t * Vec3(0.5, 0.7, 1))
    }
  }

  def hitSphere(centre: Vec3, radius: Double, ray: Ray): Boolean = {
    val oc = ray.origin - centre
    val a = ray.direction.dot(ray.direction)
    val b = 2.0 * oc.dot(ray.direction)
    val c = oc.dot(oc) - (radius * radius)
    val discriminant = (b * b) - (4 * a * c)
    discriminant > 0
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
