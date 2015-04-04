XBindJoy
========

XBindJoy is a tool for making your joysticks and gamepads more
useful. It lets you use GNU Guile to bind arbitrary scheme code to
joystick button presses and joystick axis movements, and provides
function calls to send synthetic key presses, mouse clicks, and mouse
movements from those procedures. Guile makes it easy to bind hotkeys
to joystick button chords, radically change hotkey functionality at
runtime, and more.

Example uses
============

* Use your gamepad with any game, even ones that don't understand joysticks
* Bind joystick axes to mouse movement for aiming in FPS games
* Turn any joystick into a chorded keyboard + mouse combo
* Joystick macros everywhere For Great Real-Time Strategy Justice
* Bind window manager functionality to a more ergonomic input device
* Playing Minecraft? Make crouch a toggle and never fall into lava again

Try it out
==========

I'm using a weird off-brand fake ps1 controller to test this and it has
really weird axes, so this may not work very well for you without tweaking
the declarations in examples/generic.scm. Nevertheless, if you just want to
run xbindjoy, this will (I hope) work:

````bash
  sudo apt-get install libev-dev libxtst-dev guile-2.0-dev libx11-dev
  make test
````

make test runs the example script in examples/generic.scm, which serves as
a literate demonstration of a bunch of this project's features.

Disclaimer
==========

I am by no means a software engineer. This code isn't the best, and
there are probably bugs lurking in the event logic that will bite you
when you least expect it. Installing the actual library (xbindjoy.scm
and libguilexbindjoy.so) is painful. Feel free to submit a bug report
if you find anything to improve.

About
=====

* Authors:      Check the copyright notices in each file
* License:      GPL General Public License v3
