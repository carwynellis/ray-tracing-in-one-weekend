package uk.carwynellis.raytracing

/**
  * Class representing a camera which defines the parameters used to render the scene.
  *
  * @param origin position of the camera
  * @param target the point at which the camera is directed
  * @param upVector the vertical up vector for the camera
  * @param verticalFieldOfView the vertical field of view expressed in degrees
  * @param aspectRatio the aspect ratio of the image
  */
class Camera(origin: Vec3, target: Vec3, upVector: Vec3, verticalFieldOfView: Double, aspectRatio: Double) {

  val theta = verticalFieldOfView * (math.Pi/180)
  val halfHeight = math.tan(theta/2)
  val halfWidth = aspectRatio * halfHeight

  val w = (origin - target).unitVector
  val u = upVector.cross(w).unitVector
  val v = w.cross(u)

  val lowerLeftCorner = origin - (halfWidth * u) - (halfHeight * v) - w
  val horizontal      = 2 * halfWidth * u
  val vertical        = 2 * halfHeight * v

  def getRay(s: Double, t: Double) = Ray(
    origin = origin,
    direction = lowerLeftCorner + (s * horizontal) + (t * vertical) - origin
  )

}

object Camera {
  def apply(origin: Vec3, target: Vec3, upVector: Vec3, verticalFieldOfView: Double, aspectRatio: Double) =
    new Camera(origin, target, upVector, verticalFieldOfView, aspectRatio)
}
