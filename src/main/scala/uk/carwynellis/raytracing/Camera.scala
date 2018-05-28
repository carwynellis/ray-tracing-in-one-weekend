package uk.carwynellis.raytracing

// TODO - for now this is an object - may need to convert to Class later if we have any state that needs to be defined
//        at construction time.
object Camera {

  val lowerLeftCorner = Vec3(-2, -1, -1)
  val horizontal      = Vec3(4, 0, 0)
  val vertical        = Vec3(0, 2, 0)
  val origin          = Vec3(0, 0, 0)

  def getRay(u: Double, v: Double) = Ray(
    origin = origin,
    direction = lowerLeftCorner + (u * horizontal) + (v * vertical)
  )

}
