package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}

class Vec3Test extends FunSuite with Matchers {

  test("length should return vector length") {
    Vec3(1, 1, 1).length should be (math.sqrt(3))
  }

}
