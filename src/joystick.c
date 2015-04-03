/* joystick.c: joystick handling and guile wrappers for such
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

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <linux/joystick.h>
#include <libguile.h>

#include "xbindjoy.h"
#include "joystick.h"

/* ************************************************* */
/* joystick parameter discovery */

// get name
char* get_joystick_name(char* iodev) {

	int jsfd = open(iodev, O_RDONLY);
	size_t buf_size = 1024;
	char* jsname;

	jsname = malloc(buf_size * sizeof(char));
	int rvalue = ioctl(jsfd, JSIOCGNAME(buf_size * sizeof(jsname)), jsname);
	if (rvalue < 0)
		strcpy(jsname, "Unknown");

	return jsname;
}
SCM get_joystick_name_wrapper(SCM iodev) {
	char* name = get_joystick_name(scm_to_locale_string(iodev));
	SCM result = scm_from_locale_string(name);
	if (name)
		free(name);
	return result;
}

// get number of buttons
int get_joystick_num_buttons_fd(int jsfd) {
	int nbuttons;
	ioctl(jsfd, JSIOCGBUTTONS, &nbuttons);
	return nbuttons;
}
int get_joystick_num_buttons(char* iodev) {
	int jsfd = open(iodev, O_RDONLY);
	int nbuttons = get_joystick_num_buttons_fd(jsfd);
	close(jsfd);
	return nbuttons;
}
SCM get_joystick_num_buttons_wrapper(SCM iodev) {
	int nbuttons = get_joystick_num_buttons(scm_to_locale_string(iodev));
	SCM result = scm_from_int(nbuttons);
	return result;
}

// get number of axes
int get_joystick_num_axes_fd(int jsfd) {
	int naxes;
	ioctl(jsfd, JSIOCGAXES, &naxes);
	return naxes;
}
int get_joystick_num_axes(char* iodev) {
	int jsfd = open(iodev, O_RDONLY);
	int naxes = get_joystick_num_axes_fd(jsfd);
	close(jsfd);
	return naxes;
}
SCM get_joystick_num_axes_wrapper(SCM iodev) {
	int naxes = get_joystick_num_axes(scm_to_locale_string(iodev));
	SCM result = scm_from_int(naxes);
	return result;
}



/* ************************************************* */
/* button bindings */

void handle_and_dispatch_button(struct js_event e) {
	func_list_node_t* cur;
	if (e.value) {          // is a press event
		cur = bindings_press[e.number];
	} else {                // is a release event
		cur = bindings_release[e.number];
	}
	while (cur != NULL) {
		scm_call_0(cur->func);
		cur = cur->next;
	}
}

void add_binding(int key_index, int is_press, SCM function) {
	// make new linked list node
	func_list_node_t* new = malloc(sizeof(func_list_node_t));
	new->func = function;
	scm_permanent_object(new->func);

	// and push onto head of the list for that key
	if (is_press) {
		new->next = bindings_press[key_index];
		bindings_press[key_index] = new;
	} else {
		new->next = bindings_release[key_index];
		bindings_release[key_index] = new;
	}
}

SCM add_binding_wrapper(SCM key, SCM func) {
	SCM key_is_press = scm_car(key);
	SCM key_index = scm_cdr(key);

	char* action_c = scm_to_locale_string(scm_symbol_to_string(key_is_press));
	int is_press = strcmp(action_c, "press") == 0;
	free(action_c);

	int index = scm_to_int(key_index);
	
	add_binding(index, is_press, func);
}

void init_bindings(int nbuttons) {
	bindings_press = calloc(nbuttons, sizeof(func_list_node_t*));
	bindings_release = calloc(nbuttons, sizeof(func_list_node_t*));
}

SCM init_bindings_wrapper(SCM nbuttons) {
	init_bindings(scm_to_int(nbuttons));
	return SCM_BOOL_T;
}

/* ************************************************* */
/* axis mapping */

int handle_axis_event(int* axis_vals, struct js_event e) {
	axis_vals[e.number] = e.value;
}

int dispatch_axis_bindings(int* axis_vals, size_t naxes, double dt, SCM axis_func) {
	SCM output_alist = SCM_EOL;

	for (size_t i = 0; i < naxes; i++)
		output_alist = scm_acons(scm_from_int(i), scm_from_int(axis_vals[i]), output_alist);
    
	scm_call_2(axis_func, scm_from_double(dt), output_alist);
}
