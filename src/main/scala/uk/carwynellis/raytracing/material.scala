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

class Metal(albedo: Vec3, fuzziness: Double) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): Ray = {
    val reflected = Material.reflect(rayIn.direction.unitVector, record.normal)
    Ray(record.p, reflected + (fuzziness * Sphere.randomPointInUnitSphere()))
  }
}

class Dielectric(refractiveIndex: Double) extends Material(Vec3(1,1,1)) {

  override def scatter(rayIn: Ray, record: HitRecord): Ray = {
    val reflected = Material.reflect(rayIn.direction,record.normal)
    val (outwardNormal, niOverNt) =
      if (rayIn.direction.dot(record.normal) > 0) (-record.normal, refractiveIndex)
      else (record.normal, 1.0 / refractiveIndex)
    val refracted = refract(rayIn.direction, outwardNormal, niOverNt)
    if (refracted == rayIn.direction) Ray(record.p, reflected)
    else Ray(record.p, refracted)
  }

  private def refract(v: Vec3, n: Vec3, niOverNt: Double): Vec3 = {
    val unitVectorOfV = v.unitVector
    val dt = unitVectorOfV.dot(n)
    val discriminant = 1.0 - (niOverNt * niOverNt * (1 - (dt * dt)))
    if (discriminant > 0) (niOverNt * (unitVectorOfV - (n * dt))) - (n * math.sqrt(discriminant))
    else v
  }
}