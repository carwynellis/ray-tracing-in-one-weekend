package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

/**
  * TODO - factor out image writing to clean things up here.
  *      - proper error handling of file write errors
  */
object Render extends App {

  val nx = 1200
  val ny = 600

  /**
    * Compute the color for a given ray.
    *
    * @param r
    * @return
    */
  def color(r: Ray, world: Hitable): Vec3 = {
    // TODO - do we need to pass in a HitRecord?
    val hitResult = world.hit(r, 0.0, Double.MaxValue, HitRecord(0.0, Vec3(0,0,0), Vec3(0,0,0)))

    // Ray has hit a hitable object...
    if (hitResult.hit) {
      0.5 * Vec3(
        x = hitResult.record.normal.x + 1,
        y = hitResult.record.normal.y + 1,
        z = hitResult.record.normal.z + 1
      )
    }
    // ...otherwise continue to render background.
    else {
      val unitDirection = r.direction.unitVector
      val t = 0.5 * (unitDirection.y + 1)
      ((1.0 - t) * Vec3(1, 1, 1)) + (t * Vec3(0.5, 0.7, 1))
    }
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

  val world = HitableList(List(
    Sphere(Vec3(0, 0, -1), 0.5),
    Sphere(Vec3(0, -100.5, -1), 100)
  ))

  // Write PPM data
  (ny-1 to 0 by -1) foreach { j =>
    (0 until nx) foreach { i =>
      val u = i.toDouble / nx
      val v = j.toDouble / ny

      val ray = Ray(
        origin = origin,
        direction = lowerLeftCorner + (u * horizontal) + (v * vertical)
      )

      val c = color(ray, world)

      val ir = (255.99 * c.x).toInt
      val ig = (255.99 * c.y).toInt
      val ib = (255.99 * c.z).toInt

      writer.write(s"$ir $ig $ib\n")
    }
  }

  writer.close()

  println("Finished")
}
