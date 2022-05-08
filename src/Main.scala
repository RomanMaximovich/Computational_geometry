import Vector2D.{AreSegmentsCrossed, distanceToLine};

object Main
{
  val X : Array[Vector2D] = Array(Vector2D(), Vector2D(-4, 8), Vector2D(2, 16), Vector2D(18, 4))

  val fig : Array[Vector2D] = Array(new Vector2D(-8,8), new Vector2D(-4,12), new Vector2D(5,12), new Vector2D(-2,14),
    new Vector2D(0,20), new Vector2D(6,18), new Vector2D(12,12), new Vector2D(14,4), new Vector2D(20,8),
    new Vector2D(22,2), new Vector2D(16,-2), new Vector2D(8,0), new Vector2D(3,4), new Vector2D(4,0),
    new Vector2D(0,-4), new Vector2D(-4,0), new Vector2D(0,4), new Vector2D(0,8), new Vector2D(-4,4))

  def intersects (a : Vector2D, index : Int) : Boolean =
    {
      for (i <- 0 until 19)
        {
          if (AreSegmentsCrossed(X(index), a, fig(i), fig((i+1)%19))) return true
        }

      false
    }

  def distanceToSeeX1 (a : Vector2D) : Double =
    {
      if (intersects(a, 0)) distanceToLine(X(0), Vector2D(3,4), a)
      else 0
    }

  def distanceToSeeX2 (a : Vector2D) : Double =
  {
    if (intersects(a, 1)) distanceToLine(X(1), Vector2D(0,8), a)
    else 0
  }

  def distanceToSeeX3 (a : Vector2D) : Double =
  {
    if (intersects(a, 2)) distanceToLine(X(2), Vector2D(5,12), a)
    else 0
  }

  def distanceToSeeX4 (a : Vector2D) : Double =
  {
    if (intersects(a, 3)) distanceToLine(X(3), Vector2D(14,4), a)
    else 0
  }

  def distanceToSeeMax (a : Vector2D) : Double =
    {
      var s : Double = distanceToSeeX1(a)

      s = Math.max(s, distanceToSeeX2(a))

      s = Math.max(s, distanceToSeeX3(a))

      s = Math.max(s, distanceToSeeX4(a))

      s
    }

  def main(args : Array[String]) : Unit =
    {
      var x : Vector2D = Vector2D(0, 4);

      val scale : Int = 200

      for (i <- 0 to scale*14)
        {
          for (j <- scale*4 to scale*12)
            {
              if (distanceToSeeMax(Vector2D(i/scale, j/scale)) < distanceToSeeMax(x)) x = Vector2D(i/scale,j/scale)
            }
        }

      print(x.x + "  " + x.y)
    }
}
