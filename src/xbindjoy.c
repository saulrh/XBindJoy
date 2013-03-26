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
#include "sender.h"

void debug_print(const char* msg);


/* C code structure: */
/*   xbindjoy contains the event loop and as little else as possible */
/*   joystick contains functions for handling joystick data */
/*   sender contains functions for sending x events */

/* stuff you can do in scheme:
 * (xbindjoy-send-key '('press "K")) ; sends a keyup K
 * (xbindjoy-send-key '('release "K")) ; sends a keydown K
 * (xbindjoy-send-button '('press 1)) ; presses mouse button 1
 * (xbindjoy-send-button '('release 1)) ; releases mouse button 1
 * (xbindjoy-send-mouserel x y) ; moves the mouse to a position (x, y) relative to the current pos
 * (xbindjoy-send-mouseabs x y) ; moves the mouse to a position (x, y) on the screen
 */


/* main event loop: call this and it will run forever, listening for
 * joystick inputs and running your code when it gets them */
void joystick_loop(SCM joysticks) {
    /* open joystick devices */
    /* main loop */
    while(1) {
        /*   sleep until next tick */
        /*   read all the joystick devices to grab any new events */
        /*   dispatch button state changes to handlers */
        /*   dispatch dt and axis state to each axis handler */
    }
    /* end loop */
}

void inner_main(void* data, int argc, char** argv) {
    /* for going from joystick names to devices or vice versa */
    scm_c_define_gsubr("device->jsname", 1, 0, 0, get_joystick_name_wrapper);

    /* for sending x events to the screen */
    scm_c_define_gsubr("xbindjoy-send-key", 1, 0, 0, send_key_wrapper);
    scm_c_define_gsubr("xbindjoy-send-button", 1, 0, 0, send_button_wrapper);
    scm_c_define_gsubr("xbindjoy-send-mouserel", 2, 0, 0, send_mouserel_wrapper);
    scm_c_define_gsubr("xbindjoy-send-mouseabs", 2, 0, 0, send_mouseabs_wrapper);

    /* and the loop */
    scm_c_define_gsubr("xbindjoy-start", 1, 0, 0, joystick_loop);

    /* and finally start reading lisp */
    scm_shell(argc, argv);
}

int main(int argc, char** argv) {
    /* set up variables */
    verbose = 1;
    display = XOpenDisplay(getenv("DISPLAY"));

    /* boot up guile */
    scm_boot_guile(argc, argv, inner_main, NULL);
}
