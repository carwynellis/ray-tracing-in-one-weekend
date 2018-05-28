package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}

class HitableListTest extends FunSuite with Matchers {

  case class DummyHitable(hit: Boolean) extends Hitable {
    override def hit(r: Ray, tMin: Double, tMax: Double, hitRecord: HitRecord): HitResult = HitResult(hit, hitRecord)
  }

  // For now just exercise hit detection without validating closest calculations.
  test("should return hitResult with hit = false for list of hitables with no hits") {
    val hitables = List(DummyHitable(false), DummyHitable(false), DummyHitable(false))

    val result = HitableList(hitables).hit(
      r = Ray(Vec3(0, 0, 0), Vec3(1, 1, 1)),
      tMin = 0.0,
      tMax = 1.0,
      hitRecord = HitRecord(1.0, Vec3(0, 0, 0), Vec3(1, 1, 1))
    )

    result.hit should be(false)
  }

  test("should return hitResult with hit = true for list of hitables with one hit object") {
    val hitables = List(DummyHitable(false), DummyHitable(true), DummyHitable(false))

    val result = HitableList(hitables).hit(
      r = Ray(Vec3(0, 0, 0), Vec3(1, 1, 1)),
      tMin = 0.0,
      tMax = 1.0,
      hitRecord = HitRecord(1.0, Vec3(0, 0, 0), Vec3(1, 1, 1))
    )

    result.hit should be(true)
  }

}
