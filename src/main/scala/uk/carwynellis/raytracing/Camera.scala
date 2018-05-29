package uk.carwynellis.raytracing

import scala.annotation.tailrec

/**
  * Class representing a camera which defines the parameters used to render the scene.
  *
  * @param origin position of the camera
  * @param target the point at which the camera is directed
  * @param upVector the vertical up vector for the camera
  * @param verticalFieldOfView the vertical field of view expressed in degrees
  * @param aspectRatio the aspect ratio of the image
  */
class Camera(origin: Vec3,
             target: Vec3,
             upVector: Vec3,
             verticalFieldOfView: Double,
             aspectRatio: Double,
             aperture: Double,
             focusDistance: Double
            ) {

  private val lensRadius = aperture / 2

  private val theta = verticalFieldOfView * (math.Pi/180)
  private val halfHeight = math.tan(theta/2)
  private val halfWidth = aspectRatio * halfHeight

  private val w = (origin - target).unitVector
  private val u = upVector.cross(w).unitVector
  private val v = w.cross(u)

  private val lowerLeftCorner =
    origin - (halfWidth * focusDistance * u) - (halfHeight * focusDistance * v) - (focusDistance * w)

  private val horizontal = 2 * halfWidth * focusDistance * u
  private val vertical = 2 * halfHeight * focusDistance * v

  def getRay(s: Double, t: Double) = {

    // TODO - find a better place to define this, disk object perhaps?
    @tailrec
    def randomPointInUnitDisk(): Vec3 = {

      val randomPoint = 2.0 * Vec3(
        x = math.random(),
        y = math.random(),
        z = 0
      ) - Vec3(1, 1, 0)

      if (randomPoint.dot(randomPoint) >= 1.0) randomPointInUnitDisk()
      else randomPoint
    }

    val rd = lensRadius * randomPointInUnitDisk()
    val offset = u * rd.x * rd.y

    Ray(
      origin = origin + offset,
      direction = lowerLeftCorner + (s * horizontal) + (t * vertical) - origin - offset
    )
  }

}

object Camera {
  def apply(origin: Vec3,
            target: Vec3,
            upVector: Vec3,
            verticalFieldOfView: Double,
            aspectRatio: Double,
            aperture: Double,
            focusDistance: Double) =
    new Camera(origin, target, upVector, verticalFieldOfView, aspectRatio, aperture, focusDistance)
}
