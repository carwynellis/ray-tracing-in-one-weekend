package uk.carwynellis.raytracing

/**
  * Class representing a camera which defines the parameters used to render the scene.
  *
  * @param verticalFieldOfView Vertical field of view express in degrees
  * @param aspectRatio aspect ratio of the image where aR = w / h
  */
class Camera(verticalFieldOfView: Double, aspectRatio: Double) {

  val theta = verticalFieldOfView * (math.Pi/180)
  val halfHeight = math.tan(theta/2)
  val halfWidth = aspectRatio * halfHeight

  val lowerLeftCorner = Vec3(-halfWidth, -halfHeight, -1)
  val horizontal      = Vec3(2 * halfWidth, 0, 0)
  val vertical        = Vec3(0, 2 * halfHeight, 0)
  val origin          = Vec3(0, 0, 0)

  def getRay(u: Double, v: Double) = Ray(
    origin = origin,
    direction = lowerLeftCorner + (u * horizontal) + (v * vertical)
  )

}

object Camera {
  def apply(verticalFieldOfView: Double, aspectRatio: Double) = new Camera(verticalFieldOfView, aspectRatio)
}
