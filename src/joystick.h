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


// types to use for storing the button event callbacks

// linked list of procedures.
struct func_list_node {
	struct func_list_node* next;
	SCM func;
};
typedef struct func_list_node func_list_node_t;

// array of linked lists of procedures, one per button event. So '(release . 0) has its own linked
// list of procedures, and when that action comes up we traverse the linked list calling all the
// procedures.
func_list_node_t** bindings_press;
func_list_node_t** bindings_release;


/* functions */
char* get_joystick_name(char* iodev);
SCM get_joystick_name_wrapper(SCM iodev);

int get_joystick_num_buttons_fd(int jsfd);
int get_joystick_num_buttons(char* iodev);
SCM get_joystick_num_buttons_wrapper(SCM iodev);

int get_joystick_num_axes_fd(int jsfd);
int get_joystick_num_axes(char* iodev);
SCM get_joystick_num_axes_wrapper(SCM iodev);

void init_bindings(int nbuttons);
SCM init_bindings_wrapper(SCM nbuttons);
void add_binding(int key_index, int is_press, SCM function);
SCM add_binding_wrapper(SCM key, SCM func);

void handle_and_dispatch_button(struct js_event e);

int handle_axis(int* axis_vals, struct js_event e);
int dispatch_axis_bindings(int* axis_vals, size_t naxes, double dt, SCM axis_func);
