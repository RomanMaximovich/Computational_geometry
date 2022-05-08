import Vector2D.between

case class Triangle(v1 : Vector2D, v2 : Vector2D, v3 : Vector2D)
{
  def area() : Double = ((v2.x - v1.x) * (v3.y - v1.y) - (v2.y - v1.y) * (v3.x - v1.x)) / 2

  def direction() : Int = Math.signum(area()).toInt


}

object Triangle
{
  def isInTriangle(T: Triangle, a: Vector2D): Boolean =
  {
    val T1 = new Triangle(new Vector2D(T.v1.x, T.v1.y), new Vector2D(T.v2.x, T.v2.y), new Vector2D(a.x, a.y))
    val T2 = new Triangle(new Vector2D(T.v2.x, T.v2.y), new Vector2D(T.v3.x, T.v3.y), new Vector2D(a.x, a.y))
    val T3 = new Triangle(new Vector2D(T.v3.x, T.v3.y), new Vector2D(T.v1.x, T.v1.y), new Vector2D(a.x, a.y))

    if (T1.area() == 0 || T2.area() == 0 || T3.area() == 0)
    {
      if (T1.area() == 0) return between(T.v1, T.v2, a)

      if (T2.area() == 0) return between(T.v2, T.v3, a)

      if (T3.area() == 0) return between(T.v3, T.v1, a)

      true
    }
    else if (T1.direction() == T2.direction() && T3.direction() == T2.direction()) true else false
  }
}
