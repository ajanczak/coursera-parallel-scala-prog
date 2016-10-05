import common._
import barneshut.conctrees._

import scala.annotation.tailrec

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    //size is the length of the side of the cell,
    def size: Float

    //total number of bodies in the cell.
    def total: Int

    def insert(b: Body): Quad

    def isInside(body: Body): Boolean = {
      (centerX - size / 2) >= body.x && body.x <= (centerX + size / 2) && (centerY - size / 2) >= body.y && body.y <= (centerY + size / 2)
    }

  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    val massX: Float = centerX
    val massY: Float = centerY
    val mass: Float = 0f
    val total: Int = 0

    def insert(b: Body): Quad = {
      Leaf(centerX, centerY, size, Seq(b))
    }
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val list = List(nw, ne, sw, se)
    val halfSize = (nw.size / 2)
    val centerX: Float = nw.centerX + halfSize
    val centerY: Float = nw.centerY + halfSize
    val size: Float = nw.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val total: Int = nw.total + ne.total + sw.total + se.total
    val massX: Float =
      if (total == 0) centerX
      else list.map(r => r.massX * r.centerX).sum / mass
    val massY: Float =
      if (total == 0) centerY
      else list.map(r => r.massY * r.centerY).sum / mass

    def insert(b: Body): Fork = {
      val node = list.collectFirst {
        case nodee if nodee.isInside(b) => nodee
      }.getOrElse(throw new Exception(s"Body $b Not inside $this"))
      node match {
        case e: Empty =>
          val newLeaf = e.insert(b)
          updateWithQuad(newLeaf, b)
        case f: Fork =>
          val updatedQuad = f.insert(b)
          updateWithQuad(updatedQuad, b)
        case l: Leaf =>
          updateWithQuad(l.insert(b), b)
      }
    }

    def updateWithQuad(q: Quad, b: Body): Fork = {
      b match {
        case _ if nw.isInside(b) => this.copy(nw = q)
        case _ if ne.isInside(b) => this.copy(ne = q)
        case _ if sw.isInside(b) => this.copy(sw = q)
        case _ if se.isInside(b) => this.copy(se = q)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {
    val mass = bodies.map(_.mass).sum
    val massX = bodies.map(body => body.mass * body.x).sum / mass
    val massY = bodies.map(body => body.mass * body.y).sum / mass
    val total: Int = bodies.size

    def insert(b: Body): Quad = {
      if (total < minimumSize) {
        this.copy(bodies = bodies.+:(b))
      }
      else {
        val bodiesss = bodies.+:(b)
        val sizeBy4 = (size / 4)
        val sizeBy2 = (size / 2)
        val ne = Empty(centerX - sizeBy4, centerY - sizeBy4, sizeBy2)
        val nw = Empty(centerX + sizeBy4, centerY - sizeBy4, sizeBy2)
        val se = Empty(centerX - sizeBy4, centerY + sizeBy4, sizeBy2)
        val sw = Empty(centerX + sizeBy4, centerY + sizeBy4, sizeBy2)
        bodiesss.foldLeft(Fork.apply(nw, ne, sw, se)) {
          case (quad, b) => quad.insert(b)
        }
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach { case b: Body => addForce(b.mass, b.x, b.y) }
        case f@Fork(nw, ne, sw, se) =>
          f.list.foreach { case q =>
            if (q.size / distance(q.centerX, q.centerY, x, y) < theta) addForce(q.mass, q.centerX, q.centerY)
            else traverse(q)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      ???
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      ???
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
            else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
              )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }

}
