XBindJoy
========

XBindJoy is a tool for making your joysticks and gamepads more
useful. It lets you use GNU Guile to bind arbitrary scheme code to
joystick button presses and joystick axis movements, and provides
function calls to send synthetic key presses, mouse clicks, and mouse
movements from those procedures. Guile makes it easy to bind hotkeys
to joystick button chords, radically change hotkey functionality at
runtime, and more.

How is this different from antimicro/qjoypad?
=============================================

xbindjoy is *significantly* more powerful than other available
joystick mappers. xbindjoy is configured programmatically rather than
graphically, and its bindings are full scheme procedures (function
with associated closure) rather than declarative mappings. Basically,
instead of a mapping being "button 3 is now keyboard key R", it's
"when button 3 is pressed, call function F. F is a function that sends
'keyboard key R down'". Because they're functions and closures, they
can interact and store information, so you can do things like slowly
increase mouse movement velocity by holding down one button and slowly
slow down with another. If antimicro is HTML, xbindjoy is HTML+JavaScript.

The only thing antimicro and qjoypad have that xbindjoy doesn't is
cross platform functionality. xbindjoy depends on X and the Linux
joystick API. antimicro and qjoypad may also be easier for
non-technical users to get running because they're concrete
point-and-click interfaces, though I haven't done any user experience
studies. I provide convenience functions for straightforward tasks
(things that antimicro could handle) to make those easier but it
doesn't measure up to "press button, press associated key".


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
