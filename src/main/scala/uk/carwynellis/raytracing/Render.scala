package uk.carwynellis.raytracing

object Render extends App {

  val width = 1200
  val height = 800
  val samples = 100

  val origin = Vec3(13, 2, 3)
  val target = Vec3(0, 0, 0)

  val camera = Camera(
    origin = origin,
    target = target,
    upVector = Vec3(0, 1, 0),
    verticalFieldOfView = 20,
    aspectRatio =  width.toDouble / height.toDouble,
    aperture = 0.1,
    focusDistance = 1
  )

  val filename = "image.ppm"

  println(s"Rendering scene to $filename")

  val renderer = Renderer(camera, Scene.randomScene(), width, height, samples)
  val imageWriter = ImageWriter(width, height, "image.ppm")

  renderer.renderScene().foreach(imageWriter.writePixel)

  imageWriter.close()

  println("\nFinished")
}
