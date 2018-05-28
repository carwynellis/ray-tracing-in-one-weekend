package uk.carwynellis.raytracing

import scala.annotation.tailrec

case class HitRecord(t: Double, p: Vec3, normal: Vec3)

// TODO - better name for this trait?
trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double): HitResult

}

// TODO - would something like a state monad work here instead?
case class HitResult(hit: Boolean, record: HitRecord)

class Sphere(val centre: Vec3, val radius: Double) extends Hitable {

  // TODO - get some test coverage of this method
  // TODO - refactor this, it's fugly
  override def hit(r: Ray, tMin: Double, tMax: Double): HitResult = {
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
        return HitResult(hit = true, record)
      }

      val y = (-b + discriminantRoot) / a
      if (y < tMax && y > tMin) {
        val record = HitRecord(
          t = y,
          p = r.pointAtParameter(y),
          normal = (r.pointAtParameter(y) - centre) / radius
        )
        return HitResult(hit = true, record)
      }

      // TODO - make hit record optional or get rid of hit boolean altogether
      HitResult(hit = false, HitRecord(0.0, Vec3(0,0,0), Vec3(0,0,0)))
    }
    else {
      // TODO - make hit record optional or get rid of hit boolean altogether
      HitResult(hit = false, HitRecord(0.0, Vec3(0,0,0), Vec3(0,0,0)))
    }
  }
}

object Sphere {
  def apply(centre: Vec3, radius: Double) = new Sphere(centre, radius)
}

class HitableList(val hitables: List[Hitable]) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): HitResult = {

    @tailrec
    def loop(hs: List[Hitable], closest: Double, hitAnything: Boolean, record: HitRecord): HitResult = {
      hs match {
        case x :: xs =>
          val hitResult = x.hit(r, tMin, closest)
          if (hitResult.hit)
            loop(xs, hitResult.record.t, hitAnything = true, hitResult.record)
          else loop(xs, closest, hitAnything = hitAnything, record)

        case Nil => HitResult(hitAnything, record)
      }
    }

    loop(hitables, closest = tMax, hitAnything = false, HitRecord(0.0, Vec3(0,0,0), Vec3(0,0,0)))
  }

}

object HitableList {
  def apply(hitables: List[Hitable]) = new HitableList(hitables)
}