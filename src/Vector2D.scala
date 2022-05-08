
case class Vector2D (x: Double = 0, y: Double = 0)
{
  def length (): Double = math.sqrt(x*x + y*y) // returns the length of a vector

  def wx: Int = x.toInt // casts a vector's x coordinate to a whole number

  def wy: Int = y.toInt // casts a vector's y coordinate to a whole number

  def inv: Vector2D = Vector2D(y, x) // inverts the coordinates of a vector

  def + (b: Vector2D): Vector2D = Vector2D(x+b.x, y+b.y) // adds two vectors

  def - (b: Vector2D): Vector2D = Vector2D(x - b.x, y - b.y) // subtracts one vector from the other

  def setLength (a: Double): Vector2D = this*(a/length()) // sets the length of a vector

  def ** (b : Vector2D) : Double = x*b.x + y*b.y // scalar multiplication

  def *(c : Double) : Vector2D = Vector2D(x*c, y*c) // scales a vector by a constant

  def /(c : Double) : Vector2D = Vector2D(x/c, y/c) // divides a vector by a constant

  def normalize() : Vector2D = this/this.length() // normalizes a vector
}

object Vector2D
{
  def distanceToLine(o: Vector2D, d: Vector2D, p: Vector2D): Double =
    { // finds the distance between the point p and the line passing through o and d.
      val D1 = d-o
      val P1 = p-o
      if (P1**D1 < 0)
        Math.min((o-p).length(), (d-p).length())
      else if (D1.setLength(P1**D1 / D1.length()).length() > D1.length())
             Math.min((o-p).length(), (d-p).length())
           else
             (P1 - D1.setLength(P1**D1/D1.length())).length()
    }

  def between(a: Vector2D, b: Vector2D, c: Vector2D): Boolean =
  { // finds out if the vector c is situated in the rectangle formed by vectors a and b
    c.x >= Math.min(a.x, b.x) && c.x <= Math.max(a.x, b.x) && c.y >= Math.min(a.y, b.y) && c.y <= Math.max(a.y, b.y)
  }

  def AreSegmentsCrossed(v1: Vector2D, v2: Vector2D, v3: Vector2D, v4: Vector2D): Boolean =
  { // checks if the segments [v1, v2] and [v3, v4] are crossed
    val T1 = new Triangle(v3, v4, v1)
    val T2 = new Triangle(v3, v2, v4)
    val T3 = new Triangle(v1, v3, v2)
    val T4 = new Triangle(v1, v2, v4)
    if (T1.area() == 0 && T2.area() == 0 && T3.area() == 0 && T4.area() == 0)
      !(Math.max(v1.x, v2.x) < Math.min(v3.x, v4.x)) && !(Math.max(v3.x, v4.x) < Math.min(v1.x, v2.x)) &&
        !(Math.max(v1.y, v2.y) < Math.min(v3.y, v4.y)) && !(Math.max(v3.y, v4.y) < Math.min(v1.y, v2.y))
    else
    {
      if (T1.area() == 0)
        return between(v3, v4, v1)
      if (T2.area() == 0)
        return between(v3, v4, v2)
      if (T3.area() == 0)
        return between(v1, v2, v2)
      if (T4.area() == 0)
        return between(v1, v2, v4)
      T1.direction() == T2.direction() && T3.direction() == T4.direction()
    }
  }
}
