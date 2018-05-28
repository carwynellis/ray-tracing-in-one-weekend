package uk.carwynellis.raytracing

import scala.annotation.tailrec

case class HitRecord(t: Double, p: Vec3, normal: Vec3)

// TODO - better name for this trait?
trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]

}

// TODO - would something like a state monad work here instead?
case class HitResult(hit: Boolean, record: HitRecord)

class Sphere(val centre: Vec3, val radius: Double) extends Hitable {

  // TODO - get some test coverage of this method
  // TODO - refactor and tidy up
  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val oc = r.origin - centre

    val a = r.direction.dot(r.direction)
    val b = oc.dot(r.direction)
    val c = oc.dot(oc) - (radius * radius)

    val discriminant = (b * b) - (a * c)
    val discriminantRoot = math.sqrt(discriminant)

    if (discriminant > 0) {
      val x = (-b - discriminantRoot) / a
      if (x < tMax && x > tMin) {
        val record = HitRecord(
          t = x,
          p = r.pointAtParameter(x),
          normal = (r.pointAtParameter(x) - centre) / radius
        )
        return Some(record)
      }

      val y = (-b + discriminantRoot) / a
      if (y < tMax && y > tMin) {
        val record = HitRecord(
          t = y,
          p = r.pointAtParameter(y),
          normal = (r.pointAtParameter(y) - centre) / radius
        )
        return Some(record)
      }
    }

    None
  }
}

object Sphere {
  def apply(centre: Vec3, radius: Double) = new Sphere(centre, radius)
}

class HitableList(val hitables: List[Hitable]) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {

    @tailrec
    def loop(hs: List[Hitable], closest: Double, hitAnything: Boolean, record: Option[HitRecord]): Option[HitRecord] = {
      hs match {
        case x :: xs =>
          val hitResult = x.hit(r, tMin, closest)
          hitResult match {
            case Some(updatedRecord) => loop(xs, updatedRecord.t, hitAnything = true, Some(updatedRecord))
            case None => loop(xs, closest, hitAnything = hitAnything, record)
          }
        case Nil => record
      }
    }

    loop(hitables, closest = tMax, hitAnything = false, None)
  }

}

object HitableList {
  def apply(hitables: List[Hitable]) = new HitableList(hitables)
}