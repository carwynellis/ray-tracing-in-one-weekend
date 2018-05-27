package uk.carwynellis.raytracing

/**
  * Partial implementation of the Vec3 class from chapter 2 of ray tracing in one weekend.
  *
  * May need further revisions in order to work correctly. I've tried to port the C++ code as accurately as I can.
  *
  * @param x
  * @param y
  * @param z
  */
class Vec3(x: Double, y: Double, z: Double) {

  // Alias the x, y, z values.
  val r: Double = x
  val g: Double = y
  val b: Double = z

  /**
    * Compute the vector length defined as the square root of x^2 + y^2 + z^2^
    * @return
    */
  def length = math.sqrt( (x * x) + (y * y) + (z * z))

}

object Vec3 {
  def apply(x: Double, y: Double, z: Double) = new Vec3(x, y, z)
}
