package edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {

    case Rectangle(_, _) =>
      new Location(0, 0, s)

    case Ellipse(_, _) =>
      Location(-50, -30, Rectangle(100, 60))

    case Location(x, y, shape) =>
      {
        val b = boundingBox(shape)
        Location(x + b.x, y + b.y, b.shape)
      }

    case Group(shapes @ _*) => {
      val locationList = shapes map (s1 =>
        {
          boundingBox(s1)
        })
      val s1 = locationList.foldLeft[Location](locationList.head)((a, c) => {
        val b = boundingBox(c)
        val wmax = Math.max(a.x + a.shape.asInstanceOf[Rectangle].width, b.x + b.shape.asInstanceOf[Rectangle].width)
        val wmin = Math.min(a.x, b.x)
        val width = (wmax - wmin)

        val hmax = Math.max(a.y + a.shape.asInstanceOf[Rectangle].height, b.y + b.shape.asInstanceOf[Rectangle].height)
        val hmin = Math.min(a.y, b.y)
        val height = (hmax - hmin)

        Location(a.x, a.y, (Rectangle(width, height)))

      })

      Location(s1.x, s1.y, s1.shape)

    }

  }

  def size(s: Shape): Int = s match {
    case Rectangle(_, _)   => 1
    case Ellipse(_, _)     => 1
    case Location(x, y, s) => size(s)
    case Group(shapes @ _*) => {
      val shapeList = shapes
      shapeList.foldLeft(0)((a, s1) => size(s1) + a)
    }
  }
}

