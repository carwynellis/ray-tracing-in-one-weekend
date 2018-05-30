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

  def generateRandomScene(): HitableList = {

    val lowerBound = -12
    val upperBound = 12

    val range = lowerBound until upperBound

    // TODO - refactor - this is a rough port of the C++ code
    def generateSpheres: List[Sphere] = range.flatMap { a =>
      range.flatMap { b =>
        val materialSelector = math.random()

        val centre = Vec3(
          x = a + 0.9 * math.random(),
          y = 0.2,
          z = b + 0.9 * math.random()
        )
        generateSphere(centre, materialSelector)
      }
    }.toList

    // TODO - refactor as above
    def generateSphere(c: Vec3, m: Double): Option[Sphere] = if ((c - Vec3(4, 0.2, 0)).length > 0.9) {
      if (m < 0.8)
        Some(Sphere(c, 0.2, Lambertian(Vec3(math.random(), math.random(), math.random()))))
      else if (m < 0.95) {
        def randomColor = 0.5 * (1 + math.random())
        Some(Sphere(c, 0.2,
          Metal(
            Vec3(randomColor, randomColor, randomColor),
            0.5 * math.random()
          )
        ))
      }
      else Some(Sphere(c, 0.2, Dielectric(1.5)))
    }
    else None

    val scene = List(
      Sphere(Vec3(0, -1000, 0), 1000, Lambertian(Vec3(0.5, 0.5, 0.5))),
      Sphere(Vec3(0, 1, 0), 1, Dielectric(1.5)),
      Sphere(Vec3(-4, 1, 0), 1, Lambertian(Vec3(0.4, 0.2, 0.1))),
      Sphere(Vec3(4, 1, 0), 1, Metal(Vec3(0.7, 0.6, 0.5), 0))
    ) ++ generateSpheres
    HitableList(scene)
  }

  def staticScene() = HitableList(List(
    Sphere(Vec3(0, 0, -1), 0.5, Lambertian(Vec3(0.5, 0.5, 0.6))),
    Sphere(Vec3(0, -100.5, -1), 100, Lambertian(Vec3(0.8, 0.8, 0.0))),
    Sphere(Vec3(1, 0, -1), 0.5, Metal(Vec3(0.8, 0.6, 0.2), 0.3)),
    Sphere(Vec3(-1, 0, -1), -0.5, Dielectric(1.5))
  ))

  // Write PPM header
  writer.write(
    s"""P3
      |$nx
      |$ny
      |255
      |""".stripMargin)

  val world = generateRandomScene()

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

      val ir = (255.99 * gammaCorrected.x).toInt
      val ig = (255.99 * gammaCorrected.y).toInt
      val ib = (255.99 * gammaCorrected.z).toInt

      writer.write(s"$ir $ig $ib\n")
    }
  }

  writer.close()

  println("\nFinished")
}
