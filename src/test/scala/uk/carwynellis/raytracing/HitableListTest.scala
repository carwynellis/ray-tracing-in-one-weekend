package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}

class HitableListTest extends FunSuite with Matchers {

  class DummyMaterial() extends Material(Vec3(1,1,1)) {
    override def scatter(rayIn: Ray, record: HitRecord): Ray = ???
  }

  case class DummyHitable(hit: Boolean) extends Hitable {
    override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] =
      if (hit) Some(HitRecord(0.0, Vec3(0,0,0), Vec3(0,0,0), new DummyMaterial()))
      else None
  }

  // For now just exercise hit detection without validating closest calculations.
  test("should return hitResult with hit = false for list of hitables with no hits") {
    val hitables = List(DummyHitable(false), DummyHitable(false), DummyHitable(false))

    val result = HitableList(hitables).hit(
      r = Ray(Vec3(0, 0, 0), Vec3(1, 1, 1)),
      tMin = 0.0,
      tMax = 1.0
    )

    result shouldBe empty
  }

  test("should return hitResult with hit = true for list of hitables with one hit object") {
    val hitables = List(DummyHitable(false), DummyHitable(true), DummyHitable(false))

    val result = HitableList(hitables).hit(
      r = Ray(Vec3(0, 0, 0), Vec3(1, 1, 1)),
      tMin = 0.0,
      tMax = 1.0
    )

    result shouldBe defined
  }

}
