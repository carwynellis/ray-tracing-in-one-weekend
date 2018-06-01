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
  val ny = 800
  val ns = 100

  val origin = Vec3(13, 2, 3)
  val target = Vec3(0, 0, 0)

  val camera = Camera(
    origin = origin,
    target = target,
    upVector = Vec3(0, 1, 0),
    verticalFieldOfView = 20,
    aspectRatio =  nx.toDouble / ny.toDouble,
    aperture = 0.1,
    focusDistance = 10
  )

  def renderPixel(x: Int, y: Int, world: HitableList): Vec3 = {
    @tailrec
    def loop(remaining: Int, acc: Vec3): Vec3 = {
      if (remaining > 0) {
        // TODO - better names for xR and yR?
        val xR = (x.toDouble + math.random()) / nx
        val yR = (y.toDouble + math.random()) / ny
        val ray = camera.getRay(xR, yR)
        loop(remaining - 1, acc + color(ray, world, 0))
      }
      else acc / ns
    }
    loop(ns, Vec3(0, 0, 0))
  }

  val world = Scene.randomScene()

  val imageWriter = ImageWriter(nx, ny, "image.ppm")

  // Write PPM data
  (ny-1 to 0 by -1) foreach { j =>

    // Basic progress indication per iamge scanline.
    val percentComplete = 100 - ((j.toDouble / ny) * 100)
    printf("\r% 4d%s complete", percentComplete.toInt, "%")

    (0 until nx) foreach { i =>

      val c = renderPixel(i, j, world)

      // Gamma correct the current pixel using gamma2 e.g. sqrt of each component.
      val gammaCorrected = Vec3(
        x = math.sqrt(c.x),
        y = math.sqrt(c.y),
        z = math.sqrt(c.z)
      )

      imageWriter.writePixel(
        r = (255.99 * gammaCorrected.x).toInt,
        g = (255.99 * gammaCorrected.y).toInt,
        b = (255.99 * gammaCorrected.z).toInt
      )

    }
  }

  imageWriter.close()

  println("\nFinished")
}
