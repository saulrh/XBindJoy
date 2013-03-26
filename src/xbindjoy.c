/* xbindjoy.c: event loop and some basic functions for the xbindjoy guile module
 *
 * Copyright 2013 Saul Reynolds-Haertle.
 *
 * This file is part of XBindJoy. 
 * 
 * XBindJoy is free software: you can redistribute it and/or modify 
 * it under the terms of the GNU General Public License as published by 
 * the Free Software Foundation, either version 3 of the License, or 
 * (at your option) any later version. 
 * 
 * XBindJoy is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 * GNU General Public License for more details. 
 *
 * You should have received a copy of the GNU General Public License 
 * along with XBindJoy.  If not, see <http://www.gnu.org/licenses/>. */


/* #include <config.h> */
#include <libguile.h>

#include "xbindjoy.h"
#include "joystick.h"

void debug_print(const char* msg);


/* C code structure: */
/*   xbindjoy contains the event loop and as little else as possible */
/*   joystick contains functions for handling joystick data */
/*   sender contains functions for sending x events */

/* scheme code structure:
 * (define js (jsname->device "name of joystick")) ; maps name -> /dev/input/jsN
 * (xbindjoy-button js press 1     ; bind button 1 press events on joystick js
 *                  (lambda () t)) ; to this procedure
 * (xbindjoy-axes js                    ; approximately every TIMESTEP milliseconds,
 *                (lambda (dt vals) t)) ; C calls this function with the stuff filled in
 *                                      ; vals is a list of pairs (axisnum . axisval)
 */




/* main event loop: call this and it will run forever, listening for
 * joystick inputs and running your code when it gets them */
void joystick_loop() {
    /* open joystick devices */
    /* main loop */
    /*   sleep until next tick */
    /*   read all the joystick devices to grab any new events */
    /*   dispatch button state changes to handlers */
    /*   dispatch dt and axis state to each axis handler */
    /* end loop */
}

void init_xbindjoy() {
    verbose = 1;
    scm_c_define_gsubr("device->jsname", 1, 0, 0, get_joystick_name_wrapper);
}
