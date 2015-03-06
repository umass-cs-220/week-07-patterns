package cs220

object Garage {

  trait Command {
    def execute: Unit
  }

  // All about lights:
  class Light(label: String) {
    def on = println(s"$label: light turned on")
    def off = println(s"$label: light turned off")
  }

  class LightOnCommand(light: Light) extends Command {
    def execute {
      light.on
    }
  }

  class LightOffCommand(light: Light) extends Command {
    def execute {
      light.off
    }
  }
  
  // ADDITION
  // All about garage doors
  class GarageDoor(label: String) {
    val light = new Light("garage door light")

    def up = println(s"$label: Garage door is up")
    def down = println(s"$label: Garage door is down")
    def stop = println(s"$label: Garage door is stopped")
    def lightOn = light.on
    def lightOff = light.off
  }  

  // ADDITION
  // Commands to manipulate the garage door:
  class GarageDoorOpenCommand(door: GarageDoor) extends Command {
    def execute {
      door.up
      door.lightOn
    }
  }

  class GarageDoorCloseCommand(door: GarageDoor) extends Command {
    def execute {
      door.down
      door.lightOff
    }
  }
  

  class SimpleRemoteControl(cmd: Command) {
    def press = cmd.execute
  }

  // A remote for a light:
  val light    = new Light("bedroom light 1")
  val lightOn  = new LightOnCommand(light)
  val lightOff = new LightOffCommand(light)
  val remote1  = new SimpleRemoteControl(lightOn)
  val remote2  = new SimpleRemoteControl(lightOff)

  // A remote for a garage door:
  val door      = new GarageDoor("garage door 1")
  val doorOpen  = new GarageDoorOpenCommand(door)
  val doorClose = new GarageDoorCloseCommand(door)
  val remote3   = new SimpleRemoteControl(doorOpen)
  val remote4   = new SimpleRemoteControl(doorClose)

}
