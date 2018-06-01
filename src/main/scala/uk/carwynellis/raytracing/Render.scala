package uk.carwynellis.raytracing

import java.io.{File, PrintWriter}

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
    focusDistance = 1
  )

  /**
    * Sample a number of randomly generated rays for the current pixel.
    *
    * Higher sample counts yield a better quality image at the expense of longer render times.
    * @param x
    * @param y
    * @param world
    * @return
    */
  def renderPixel(x: Int, y: Int, world: HitableList): Vec3 =
    (0 until ns).map { _ =>
      val xR = (x.toDouble + math.random()) / nx
      val yR = (y.toDouble + math.random()) / ny
      val ray = camera.getRay(xR, yR)
      color(ray, world, 0)
    }.reduce(_ + _) / ns

  val world = Scene.randomScene()

  val imageWriter = ImageWriter(nx, ny, "image.ppm")

  // Write PPM data
  val image = (ny-1 to 0 by -1) map { j =>
    // Basic progress indication per image scanline.
    val percentComplete = 100 - ((j.toDouble / ny) * 100)
    printf("\r% 4d%s complete", percentComplete.toInt, "%")

    (0 until nx) map { i =>
      renderPixel(i, j, world).toPixel
    }
  }

  image.flatten.foreach(imageWriter.writePixel)

  imageWriter.close()

  println("\nFinished")
}

class Pixel(val r: Int, val g: Int, val b: Int)

object Pixel {
  def fromVec3(v: Vec3): Pixel = {
    // Gamma correct the current pixel using gamma2 e.g. sqrt of each component.
    val gammaCorrected = Vec3(
      x = math.sqrt(v.x),
      y = math.sqrt(v.y),
      z = math.sqrt(v.z)
    )

    new Pixel(
      r = (255.99 * gammaCorrected.x).toInt,
      g = (255.99 * gammaCorrected.y).toInt,
      b = (255.99 * gammaCorrected.z).toInt
    )
  }
}
