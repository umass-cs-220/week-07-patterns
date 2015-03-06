package cs220

object General {

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
  

  // All about garage doors
  class GarageDoor(label: String) {
    val light = new Light("garage door light")

    def up = println(s"$label: Garage door is up")
    def down = println(s"$label: Garage door is down")
    def stop = println(s"$label: Garage door is stopped")
    def lightOn = light.on
    def lightOff = light.off
  }  

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

  // ADDITION
  // A command that represents no command!
  object NoCommand extends Command {
    def execute { /* does nothing! */ }
  }
  
  // ADDITION
  // This is an improved remote control that allows several commands
  // to be registered with it:
  class RemoteControl(slots: Int) {
    val onbuttons: Array[Command]  = Array.fill(slots){ NoCommand }
    val offbuttons: Array[Command] = Array.fill(slots){ NoCommand }

    def setCommand(slot: Int, onCmd: Command, offCmd: Command) {
      if (slot < 0 || slot > slots)
        throw new RuntimeException("slot is not defined for this remote")
      onbuttons(slot)  = onCmd
      offbuttons(slot) = offCmd
    }

    def pushOn(slot: Int) {
      if (slot < 0 || slot > slots)
        throw new RuntimeException("slot is not defined for this remote")
      onbuttons(slot).execute
    }

    def pushOff(slot: Int) {
      if (slot < 0 || slot > slots)
        throw new RuntimeException("slot is not defined for this remote")
      offbuttons(slot).execute
    }

    override def toString = {
      val list =
        for {
          i <- 0 to (slots - 1)
          on  = onbuttons(i).getClass.getName
          off = offbuttons(i).getClass.getName
          str = "On: " + on + ", Off: " + off
        } yield str
      "\n================ Remote Control ================\n" + list
    }

}  

  // Light with commands:
  val light    = new Light("bedroom light 1")
  val lightOn  = new LightOnCommand(light)
  val lightOff = new LightOffCommand(light)

  // Garage door with commands:
  val door      = new GarageDoor("garage door 1")
  val doorOpen  = new GarageDoorOpenCommand(door)
  val doorClose = new GarageDoorCloseCommand(door)

  // ADDITION: create the new remote
  val remote = new RemoteControl(2)
  remote.setCommand(0, lightOn, lightOff)
  remote.setCommand(1, doorOpen, doorClose)
}
