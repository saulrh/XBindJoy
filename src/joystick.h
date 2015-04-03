/* joystick.h: joystick handling and guile wrappers for such
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

#include <linux/joystick.h>
#include "xbindjoy.h"


/* data structures for binding keys */
typedef struct {
	int key_index;
	int is_press;
	SCM function;
} bind_key_t;

typedef struct {
	size_t nkeys;
	bind_key_t* keys;
} keymap_t;


/* functions */
char* get_joystick_name(char* iodev);
SCM get_joystick_name_wrapper(SCM iodev);

int get_joystick_num_axes_fd(int jsfd);
int get_joystick_num_axes(char* iodev);
SCM get_joystick_num_axes_wrapper(SCM iodev);


keymap_t* build_keymap_from_scm_alist(SCM kmap_alist);
int handle_and_dispatch_keys(keymap_t* kmap, struct js_event e);

int handle_axis(int* axis_vals, struct js_event e);
int dispatch_axes(int* axis_vals, size_t naxes, double dt, SCM axis_func);
