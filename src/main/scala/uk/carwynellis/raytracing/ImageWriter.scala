package uk.carwynellis.raytracing
import scala.collection.immutable

/**
  * Initial attempt at a PPM writer from Chapter 1.
  *
  * This pretty much just ports directly what's there in the C++ snippet including the original variable names.
  */
object ImageWriter extends App {

  val nx = 200
  val ny = 100

  // Write PPM header
  println(
    s"""P3
      |$nx
      |$ny
      |255""".stripMargin)

  // Write PPM data
  (ny-1 to 0 by -1) foreach { j =>
    (0 until nx) foreach { i =>
      val r =  i.toFloat / nx
      val g = j.toFloat / ny
      val b = 0.2F

      val ir = (255.99*r).toInt
      val ig = (255.99*g).toInt
      val ib = (255.99*b).toInt

      println(s"$ir $ig $ib")
    }
  }

}
