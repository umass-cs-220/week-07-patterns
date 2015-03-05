package cs220

trait Beverage {
  def getDescription: String
  def cost: Double
}

// Concrete Components

class HouseBlend extends Beverage {
  def getDescription: String = "HouseBlend Coffee"
  def cost: Double = 3.25
}

class DarkRoast extends Beverage {
  def getDescription: String = "Darkroast Coffee"
  def cost: Double = 3.75
}

class Espresso extends Beverage {
  def getDescription: String = "Espresso"
  def cost: Double = 1.25
}

class Decaf extends Beverage {
  def getDescription: String = "Decaf Coffee"
  def cost: Double = 2.25
}

// Decorators

abstract class CondimentDecorator(b: Beverage) extends Beverage {
  val beverage = b
}

case class Milk(b: Beverage) extends CondimentDecorator(b) {
  def getDescription: String = b.getDescription + " with Milk"
  def cost: Double = 0.25 + b.cost
}

case class Mocha(b: Beverage) extends CondimentDecorator(b) {
  def getDescription: String = b.getDescription + " with Mocha"
  def cost: Double = 0.75 + b.cost
}

case class Soy(b: Beverage) extends CondimentDecorator(b) {
  def getDescription: String = b.getDescription + " with Soy"
  def cost: Double = 1.15 + b.cost
}

case class Whip(b: Beverage) extends CondimentDecorator(b) {
  def getDescription: String = b.getDescription + " with Whip"
  def cost: Double = 1.15 + b.cost
}