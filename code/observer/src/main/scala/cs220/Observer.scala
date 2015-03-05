package cs220


trait Subject {
  def registerObserver(o: Observer): Unit
  def removeObserver(o: Observer): Unit
  def notifyObservers: Unit
}


trait Observer {
  def update(temp: Double, humidity: Double, pressure: Double)
}


trait DisplayElement {
  def display: Unit
}


class WeatherData extends Subject {
  var observers: List[Observer] = List()
  var temperature: Double = 0.0
  var humidity: Double = 0.0
  var pressure: Double = 0.0

  def registerObserver(o: Observer): Unit =
    observers = o :: observers

  def removeObserver(o: Observer): Unit =
    observers = observers.filterNot(_ == o)

  def notifyObservers: Unit =    
    observers.foreach(_.update(temperature, humidity, pressure))

  def measurementsChanged =
    notifyObservers

  def setMeasurements(t: Double, h: Double, p: Double) = {
    temperature = t
    humidity = h
    pressure = p
    measurementsChanged
  }
}


class CurrentConditionsDisplay(weatherdata: Subject) extends Observer with DisplayElement {
  val subject = weatherdata
  var temperature: Double = 0.0
  var humidity: Double = 0.0

  // Register this display as an observer:
  weatherdata.registerObserver(this)

  def update(temp: Double, humidity: Double, pressure: Double) = {
    this.temperature = temp
    this.humidity    = humidity
    display
  }

  def display: Unit = {
    println("Current conditions: " + temperature + "F degrees and " + humidity + "% humidity")
  }
}


class ForecastDisplay(weatherdata: Subject) extends Observer with DisplayElement {
  val subject = weatherdata
  var temperature: Double = 0.0
  var humidity: Double = 0.0

  // Register this display as an observer:
  weatherdata.registerObserver(this)

  def update(temp: Double, humidity: Double, pressure: Double) = {
    this.temperature = temp
    this.humidity    = humidity
    display
  }

  def display: Unit = {
    if (temperature < 50)
      println("Forecast: a bit cold outside")
    else if (temperature < 70)
      println("Forecast: it is a comfortable day")
    else if (temperature < 85)
      println("Forecast: it is a perfect day")
    else
      println("Forecast: it is HOT!")
  }
}