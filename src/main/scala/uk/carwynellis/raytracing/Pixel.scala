package uk.carwynellis.raytracing

class Pixel(val r: Int, val g: Int, val b: Int)

object Pixel {
  def fromVec3(v: Vec3): Pixel = {
    // Gamma correct the current pixel using gamma2 e.g. sqrt of each component.
    val gammaCorrected = Vec3(
      x = math.sqrt(v.x),
      y = math.sqrt(v.y),
      z = math.sqrt(v.z)
    )

    new Pixel(
      r = (255.99 * gammaCorrected.x).toInt,
      g = (255.99 * gammaCorrected.y).toInt,
      b = (255.99 * gammaCorrected.z).toInt
    )
  }
}
