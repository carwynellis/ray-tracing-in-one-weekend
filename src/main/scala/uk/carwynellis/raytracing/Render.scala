package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

import scala.annotation.tailrec

/**
  * TODO - factor out image writing to clean things up here.
  *      - proper error handling of file write errors
  */
object Render extends App {

  // When rendering some rays may may include a floating point error preventing them from being treated as 0.
  // We increase the minimum value we accept slight which yields a smoother image without visible noise.
  val ImageSmoothingLimit = 0.001

  /**
    * Compute the color for a given ray.
    *
    * @param r
    * @return
    */
  def color(r: Ray, world: Hitable, depth: Int): Vec3 = {

    val hitResult = world.hit(r, ImageSmoothingLimit, Double.MaxValue)

    hitResult match {
      case Some(hit) =>
        if (depth < 50) {
          val scattered = hit.material.scatter(r, hit)
          hit.material.albedo * color(scattered, world, depth + 1)
        }
        else Vec3(0, 0, 0)
      case None =>
        val unitDirection = r.direction.unitVector
        val t = 0.5 * (unitDirection.y + 1)
        ((1.0 - t) * Vec3(1, 1, 1)) + (t * Vec3(0.5, 0.7, 1))
    }

  }

  val fileName = "image.ppm"
  val writer = new PrintWriter(new File(fileName))

  println(s"Rendering image to $fileName")

  val nx = 1200
  val ny = 600
  val ns = 100

  def renderPixel(x: Int, y: Int, world: HitableList): Vec3 = {
    @tailrec
    def loop(remaining: Int, acc: Vec3): Vec3 = {
      if (remaining > 0) {
        // TODO - better names for xR and yR?
        val xR = (x.toDouble + math.random()) / nx
        val yR = (y.toDouble + math.random()) / ny
        val ray = Camera.getRay(xR, yR)
        loop(remaining - 1, acc + color(ray, world, 0))
      }
      else acc / ns
    }
    loop(ns, Vec3(0, 0, 0))
  }

  // Write PPM header
  writer.write(
    s"""P3
      |$nx
      |$ny
      |255
      |""".stripMargin)

  val world = HitableList(List(
    Sphere(Vec3(0, 0, -1), 0.5, new Lambertian(Vec3(0.1, 0.2, 0.5))),
    Sphere(Vec3(0, -100.5, -1), 100, new Lambertian(Vec3(0.8, 0.8, 0.0))),
    Sphere(Vec3(1, 0, -1), 0.5, new Metal(Vec3(0.8, 0.6, 0.2), 0.3)),
    Sphere(Vec3(-1, 0, -1), 0.5, new Dielectric(1.5))
  ))

  // Write PPM data
  (ny-1 to 0 by -1) foreach { j =>
    (0 until nx) foreach { i =>

      val c = renderPixel(i, j, world)

      // Gamma correct the current pixel using gamma2 e.g. sqrt of each component.
      val gammaCorrected = Vec3(
        x = math.sqrt(c.x),
        y = math.sqrt(c.y),
        z = math.sqrt(c.z)
      )

      val ir = (255.99 * gammaCorrected.x).toInt
      val ig = (255.99 * gammaCorrected.y).toInt
      val ib = (255.99 * gammaCorrected.z).toInt

      writer.write(s"$ir $ig $ib\n")
    }
  }

  writer.close()

  println("Finished")
}
