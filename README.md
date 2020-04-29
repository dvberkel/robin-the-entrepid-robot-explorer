# Robin the entrepid robot explorer
This is an exploration in parsing with `elm/parser`.

## Start the exploration
In order to start this exploration clone this repository with the following command

```sh
git clone https://github.com/fifth-postulate/robin-the-entrepid-robot-explorer.git
```

Change directory and fire up `elm reactor` and read on

### Workhop
This is a workshop on learning [`elm/parser`][elm/parser]. You learn it creating a parser for a ficticious programming language. But an important language none the less. For it allows you to save Robin the entrepid robot explorer.

The workshop is divided into different levels. You will write code as you go, so feel free to apply all the healthy habits you acquired for writing good code. If at any moment you are stuck, don't hesitate to ask questions.

Each level has a description that you find in this README. So don't forget to read it before diving in.

## Level 0
**Krzzthpff** _Kshshshshshshshshshshsh_

You look up from you work and are surprised that you are hearing only static over the communication channel. What just happened?

You roll over to the bank of controls, your eyes darting over the various status monitors. Something is seriously wrong. You cut the static and wind back the CCTV feed a few seconds.

There! A bright flash illuminates your face. With sleight unease you stop the rewind and play the CCTV footage forward. You clearly see Robin the entrepid robot explorer going on her autonomous business when some sort of electric discharge lights up the screen.

You frantically type in some commands and a status report is shown on screen.

```toml
[status]
Artificial Intelligence   = "down"
Route Planner             = "down"
Global Positioning System = "down"
Communications            = "down"
Core Memory               = "down"
Central Processing Unit   = "down"
Motor Driver Processor    = "operational"
Controller Area Network   = "operational"
Motor Control             = "operational"
Sensors                   = "operational"
```

Wow, the surge has wiped almost all highlevel functions. With communications down, there is no way of restoring Robin the entrepid robot explorer back to an operational state. She needs to come back to the lab, so she can be repaired.

Luckily the Motor Driver Processor and Controller Area Network are still operational. This way you can update the motor driver to respond to commands send over the Controller Area Network!

Quick! Patch a connection to CCTV feed and [hack into the robot control](http://localhost:8000/src/ControlRoom.elm).

[elm/parser]: https://package.elm-lang.org/packages/elm/parser/latest/ 