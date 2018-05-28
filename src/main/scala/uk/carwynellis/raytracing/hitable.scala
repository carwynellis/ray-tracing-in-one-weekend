package uk.carwynellis.raytracing

case class HitRecord(t: Double, p: Vec3, normal: Vec3)

// TODO - better name for this trait?
trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): Boolean

}

class Sphere(val centre: Vec3, val radius: Double) extends Hitable {

  // TODO - get some test coverage of this method
  // TODO - refactor this, it's ugly
  override def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): Boolean = {
    val oc = r.origin - centre

    val a = r.direction.dot(r.direction)
    val b = oc.dot(r.direction)
    val c = oc.dot(oc) - (radius * radius)

    val discriminant = (b * b) - (a * c)

    // TODO - the C++ code seems to mutate the record that is passed in so this will probably need to change...
    if (discriminant > 0) {
      // TODO - what is this computing?
      val x = (-b - math.sqrt((b * b) - (a * c))) / a
      if (x < tMin && x > tMin) {
        // TODO - what do we do with these hit records?
        val record = HitRecord(
          t = x,
          p = r.pointAtParameter(x),
          normal = (r.pointAtParameter(x) - centre) / radius
        )
        return true
      }

      // TODO - what is this computing?
      val y = (-b + math.sqrt((b * b) - (a * c))) / a
      if (y < tMax && y > tMin) {
        val record = HitRecord(
          t = y,
          p = r.pointAtParameter(y),
          normal = (r.pointAtParameter(y) - centre) / radius
        )
        return true
      }

      false
    }
    else {
      false
    }

  }
}

object Sphere {
  def apply(centre: Vec3, radius: Double) = new Sphere(centre, radius)
}

// TODO - do we need a wrapper class like this?
class HitableList(val hitables: List[Hitable]) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): Boolean = {
    ???
  }

}

object HitableList {
  def apply(hitables: List[Hitable]) = new HitableList(hitables)
}