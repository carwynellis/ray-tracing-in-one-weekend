package uk.carwynellis.raytracing

abstract class Material(val albedo: Vec3) {

  def scatter(rayIn: Ray, record: HitRecord): Ray

}

object Material {

  def reflect(v: Vec3, n: Vec3): Vec3 = v - ( 2 * v.dot(n) * n)

}

class Lambertian(albedo: Vec3) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): Ray = {
    val target = record.p + record.normal + Sphere.randomPointInUnitSphere()
    Ray(record.p, target - record.p)
  }
}

class Metal(albedo: Vec3) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): Ray = {
    val reflected = Material.reflect(rayIn.direction.unitVector, record.normal)
    Ray(record.p, reflected)
  }
}