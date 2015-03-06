package cs220

object Simple {

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

  class SimpleRemoteControl(cmd: Command) {
    def press = cmd.execute
  }

  val light    = new Light("bedroom light 1")
  val lightOn  = new LightOnCommand(light)
  val lightOff = new LightOffCommand(light)
  val remote1  = new SimpleRemoteControl(lightOn)
  val remote2  = new SimpleRemoteControl(lightOff)
}
