package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

import scala.annotation.tailrec

/**
  * TODO - factor out image writing to clean things up here.
  *      - proper error handling of file write errors
  */
object Render extends App {

  /**
    * Compute the color for a given ray.
    *
    * @param r
    * @return
    */
  def color(r: Ray, world: Hitable): Vec3 = {

    val hitResult = world.hit(r, 0.0, Double.MaxValue)

    hitResult match {
      case Some(hit) =>
        val target = hit.p + hit.normal + Sphere.randomPointInUnitSphere()
        0.5 * color(Ray(hit.p, target - hit.p), world)
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
        loop(remaining - 1, acc + color(ray, world))
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
    Sphere(Vec3(0, 0, -1), 0.5),
    Sphere(Vec3(0, -100.5, -1), 100)
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
