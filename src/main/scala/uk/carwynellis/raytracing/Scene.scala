package uk.carwynellis.raytracing

object Scene {

  val staticScene = HitableList(List(
    Sphere(Vec3(0, 0, -1), 0.5, Lambertian(Vec3(0.5, 0.5, 0.6))),
    Sphere(Vec3(0, -100.5, -1), 100, Lambertian(Vec3(0.8, 0.8, 0.0))),
    Sphere(Vec3(1, 0, -1), 0.5, Metal(Vec3(0.8, 0.6, 0.2), 0.3)),
    Sphere(Vec3(-1, 0, -1), -0.5, Dielectric(1.5))
  ))

  def randomScene(): HitableList = {

    val lowerBound = -11
    val upperBound = 11

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

}
