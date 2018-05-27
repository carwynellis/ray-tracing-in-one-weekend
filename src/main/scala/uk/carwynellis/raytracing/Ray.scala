package uk.carwynellis.raytracing

class Ray(val origin: Vec3, val direction: Vec3) {

  def pointAtParameter(t: Double) = origin + (t * direction)

}

object Ray {
  def apply(origin: Vec3, direction: Vec3) = new Ray(origin, direction)
}
