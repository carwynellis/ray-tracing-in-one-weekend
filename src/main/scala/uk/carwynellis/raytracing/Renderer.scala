package uk.carwynellis.raytracing

import scala.annotation.tailrec

class Renderer(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) {

  // When rendering some rays may may include a floating point error preventing them from being treated as 0.
  // We increase the minimum value we accept slight which yields a smoother image without visible noise.
  val ImageSmoothingLimit = 0.001
  // Maximum number of times a ray will be bounced off objects in the scene when computing color.
  val MaxRecursionDepth = 50

  /**
    * Compute the color for a given ray.
    *
    * If the ray hits an object it is bounced in random directions until it no-longer hits an object, or the maximum
    * number of recursions is reached.
    */
  @tailrec
  private def color(r: Ray, world: Hitable, acc: Vec3, depth: Int = 0): Vec3 =
    world.hit(r, ImageSmoothingLimit, Double.MaxValue) match {
      case Some(hit) if depth < MaxRecursionDepth =>
          val scattered = hit.material.scatter(r, hit)
          color(scattered, world, hit.material.albedo * acc, depth + 1)
      case _ => acc
    }

  // Generates a gradated background color where for when a ray doesn't hit an object.
  private def backgroundColor(r: Ray) = {
    val unitDirection = r.direction.unitVector
    val t = 0.5 * (unitDirection.y + 1)
    ((1.0 - t) * Vec3(1, 1, 1)) + (t * Vec3(0.5, 0.7, 1))
  }

  /**
    * Sample a number of randomly generated rays for the current pixel.
    *
    * Higher sample counts yield a better quality image at the expense of longer render times.
    * @param x
    * @param y
    * @return
    */
  def renderPixel(x: Int, y: Int): Pixel = {
    val result = (0 until samples).map { _ =>
      val xR = (x.toDouble + math.random()) / width
      val yR = (y.toDouble + math.random()) / height
      val ray = camera.getRay(xR, yR)
      color(ray, scene, backgroundColor(ray))
    }.reduce(_ + _) / samples
    result.toPixel
  }

  /**
    * Renders the entire scene returning a list of Pixels representing the rendered scene.
    *
    * @return
    */
  def renderScene(): Seq[Pixel] = (height-1 to 0 by -1).flatMap { j: Int =>
    showProgress(j)
    (0 until width).map(renderPixel(_, j))
  }

  // Basic progress indication, updated for each horizontal line of the image.
  private def showProgress(hPos: Int): Unit = {
    val percentComplete = 100 - ((hPos.toDouble / height) * 100)
    printf("\r% 4d%s complete", percentComplete.toInt, "%")
  }

}

object Renderer {
  def apply(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) =
    new Renderer(camera, scene, width, height, samples)
}
