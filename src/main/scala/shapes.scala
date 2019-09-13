package edu.luc.cs.laufer.cs372.shapes

/**
  * data Shape = Rectangle(w, h) | Location(x, y, Shape)
  */
sealed trait Shape

case class Rectangle(width: Int, height: Int) extends Shape {
  require(height != null, "null height in Rectangle")
}

case class Ellipse(width: Int, height: Int) extends Shape {
  require(height != null, "null height in Ellipse")
}

case class Location(x: Int, y: Int, shape: Shape) extends Shape {
  require(shape != null, "null shape in Location")
}

case class Group(shape: Shape*) extends Shape {
  require(shape != null, "null shape in Group")
}

// TODO add missing case classes (see test fixtures)
// TODO must include validity checking for constructor arguments

