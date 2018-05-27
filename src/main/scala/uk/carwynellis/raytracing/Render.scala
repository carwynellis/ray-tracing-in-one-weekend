package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

/**
  * TODO - factor out image writing to clean things up here.
  *      - proper error handling of file write errors
  */
object Render extends App {

  val nx = 800
  val ny = 400

  /**
    * Compute the color for a given ray.
    *
    * @param r
    * @return
    */
  def color(r: Ray): Vec3 = {
    val result = hitSphere(Vec3(0, 0, -1), 0.5, r)

    // Ray intersects sphere so compute shading...
    if (result > 0) {
      val normal = (r.pointAtParameter(result) - Vec3(0, 0, -1)).unitVector
      0.5 * Vec3(normal.x + 1, normal.y + 1, normal.z + 1)
    }
    // ...otherwise continue to render background.
    else {
      val unitDirection = r.direction.unitVector
      val t = 0.5 * (unitDirection.y + 1)
      ((1.0 - t) * Vec3(1, 1, 1)) + (t * Vec3(0.5, 0.7, 1))
    }
  }

  def hitSphere(centre: Vec3, radius: Double, ray: Ray): Double = {
    val oc = ray.origin - centre

    val a = ray.direction.dot(ray.direction)
    val b = 2.0 * oc.dot(ray.direction)
    val c = oc.dot(oc) - (radius * radius)

    val discriminant = (b * b) - (4 * a * c)

    if (discriminant < 0) -1
    else (-b - math.sqrt(discriminant)) / (2 * a)
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
