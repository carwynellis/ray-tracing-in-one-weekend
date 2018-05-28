package uk.carwynellis.raytracing

import scala.annotation.tailrec

case class HitRecord(t: Double, p: Vec3, normal: Vec3)

// TODO - better name for this trait?
trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): HitResult

}

// TODO - would something like a state monad work here instead?
case class HitResult(hit: Boolean, record: HitRecord)

class Sphere(val centre: Vec3, val radius: Double) extends Hitable {

  // TODO - get some test coverage of this method
  // TODO - refactor this, it's fugly
  override def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): HitResult = {
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
        return HitResult(hit = true, record)
      }

      // TODO - what is this computing?
      val y = (-b + math.sqrt((b * b) - (a * c))) / a
      if (y < tMax && y > tMin) {
        val record = HitRecord(
          t = y,
          p = r.pointAtParameter(y),
          normal = (r.pointAtParameter(y) - centre) / radius
        )
        return HitResult(hit = true, record)
      }

      HitResult(hit = false, hitRecord)
    }
    else {
      HitResult(hit = false, hitRecord)
    }
  }
}

object Sphere {
  def apply(centre: Vec3, radius: Double) = new Sphere(centre, radius)
}

// TODO - do we need a wrapper class like this?
class HitableList(val hitables: List[Hitable]) extends Hitable {

  // TODO - test this method...
  override def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): HitResult = {

    @tailrec
    def loop(hs: List[Hitable], closest: Double, hitAnything: Boolean, record: HitRecord): HitResult = {
      hs match {
        case x :: xs =>
          // TODO - we need the result of the hit record...
          val hitResult = x.hit(r, tMin, closest, record)
          if (hitResult.hit) loop(xs, hitResult.record.t, hitAnything = true, hitResult.record)
          else loop(xs, record.t, hitAnything = hitAnything, record)

        case Nil => HitResult(hitAnything, record)
      }
    }

    loop(hitables, 0.0, hitAnything = false, hitRecord)
  }

}

object HitableList {
  def apply(hitables: List[Hitable]) = new HitableList(hitables)
}