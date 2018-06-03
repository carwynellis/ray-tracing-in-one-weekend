package uk.carwynellis.raytracing

class Renderer(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) {

  // When rendering some rays may may include a floating point error preventing them from being treated as 0.
  // We increase the minimum value we accept slight which yields a smoother image without visible noise.
  val ImageSmoothingLimit = 0.001

  /**
    * Compute the color for a given ray.
    *
    * TODO - do we need to pass in depth parameter?
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
      color(ray, scene, 0)
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
