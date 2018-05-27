package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}

class Vec3Test extends FunSuite with Matchers {

  test("unary - operator returns negation of Vec3") {
    -Vec3(1, 1, 1) should be(Vec3(-1, -1, -1))
  }

  test("unary + operator returns unmodified vec3 instance") {
    +Vec3(1, 1, 1) should be(Vec3(1, 1, 1))
  }

  test("length should return vector length") {
    Vec3(1, 1, 1).length should be(math.sqrt(3))
  }

  test("squaredLength should return sum of each component squared") {
    Vec3(1, 1, 1).squaredLength should be(3)
  }

  test("unitVector should return a new vector with total length 1") {
    Vec3(1, 2, 3).unitVector.length should be(1)
  }

  test("+ should return a new vector containing the sum of the two vectors") {
    Vec3(1, 2, 3) + Vec3(1, 2, 3)  should be(Vec3(2, 4, 6))
  }

  test("- should return a new vector containing the difference of the two vectors") {
    Vec3(2, 4, 6) - Vec3(1, 2, 3) should be(Vec3(1, 2, 3))
  }

  test("* should return a new vector containing the product of each component") {
    Vec3(1, 2, 3) * Vec3(2, 2, 2) should be(Vec3(2, 4, 6))
  }

  test("* by double should return a new vector with each component multiplied by that double") {
    Vec3(1, 2, 3) * 2.0 should be(Vec3(2, 4, 6))
  }

  test("double * vec3 should return a new vector with each component multiplied by that double") {
    2.0 * Vec3(1, 2, 3) should be(Vec3(2, 4, 6))
  }

  test("/ should return a new vector with each component of the first divided by the second") {
    Vec3(2, 4, 6) / Vec3(2, 2, 2) should be(Vec3(1, 2, 3))
  }

  test("Vec3 / double should return a new vector with each component divided by that double") {
    Vec3(2, 4, 6) / 2.0 should be(Vec3(1, 2, 3))
  }

  test("dot should compute correct dot product of two vectors") {
    Vec3(1, 2, 3).dot(Vec3(1, 1, 1)) should be(6)
  }

  test("cross should compute correct cross product of two vecrors") {
    Vec3(1, 2, 3).cross(Vec3(4, 5, 6)) should be(Vec3(-3, 6, -3))
  }

}
