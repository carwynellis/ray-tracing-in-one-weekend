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
case class Vec3(x: Double, y: Double, z: Double) {

  // Alias the x, y, z values.
  val r: Double = x
  val g: Double = y
  val b: Double = z

  // TODO - some operators including array indexing operators not yet implemented.
  def unary_-(): Vec3 = Vec3( -x, -y, -z)
  def unary_+(): Vec3 = this

  /**
    * Compute the vector length defined as the square root of x^2 + y^2 + z^2^
    * @return
    */
  def length: Double  = math.sqrt(squaredLength)

  /**
    * Computes the sum of each of the x,y,z components squared.
    *
    * @return
    */
  def squaredLength: Double = (x * x) + (y * y) + (z * z)

  // TODO - stream operators >> and << not yet implemented.

  /**
    * Generates a new vector with total length 1 maintaining the direction of the original vector.
    *
    * Note - this returns a new instance of the vector rather than modifying in place as per the mutable c++
    *        implementation.
    */
  def unitVector: Vec3 = this / length

  def +(that: Vec3) = Vec3(
    x = this.x + that.x,
    y = this.y + that.y,
    z = this.z + that.z
  )

  def -(that: Vec3) = Vec3(
    x = this.x - that.x,
    y = this.y - that.y,
    z = this.z - that.z
  )

  def *(that: Vec3) = Vec3(
    x = this.x * that.x,
    y = this.y * that.y,
    z = this.z * that.z
  )

  def *(n: Double) = Vec3(n * x, n * y, n * z)

  def /(that: Vec3) = Vec3(
    x = this.x / that.x,
    y = this.y / that.y,
    z = this.z / that.z
  )

  def /(n: Double) = Vec3(x / n, y / n, z / n)

  /**
    * Computes the dot product of this vector and the specified vector.
    *
    * @param that
    * @return
    */
  def dot(that: Vec3): Double = (this.x * that.x) + (this.y * that.y) + (this.z * that.z)

  /**
    * Computes the cross product of this vector and the specified vector.
    *
    * Implementation verified with https://betterexplained.com/articles/cross-product/
    *
    * @param that
    * @return
    */
  def cross(that: Vec3): Vec3 = Vec3(
    x = (this.y * that.z) - (this.z * that.y),
    y = -((this.x * that.z) - (this.z * that.x)),
    z = (this.x * that.y) - (this.y * that.x)
  )
}

object Vec3 {

  implicit class DoubleWithVec3Ops(d: Double) {
    def *(v: Vec3): Vec3 = v * d
  }

  implicit class Vec3ToPixelOps(v: Vec3) {
    def toPixel = Pixel.fromVec3(v)
  }

}
