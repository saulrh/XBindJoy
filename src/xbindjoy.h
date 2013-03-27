/* xbindjoy.h: some global variables for the entire xbindjoy program
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

#pragma once

#include<X11/Xlib.h>

int verbose;
Display* display;



typedef enum {
PRESS,
RELEASE
} key_action_e;



struct timespec axis_freq; /* the maximum allowed time between axis
                            * updates being sent back to guile for
                            * processing. This is useful because it
                            * establishes a lower limit on the
                            * frequency of mouse movements, which
                            * helps makes things smoother. */
