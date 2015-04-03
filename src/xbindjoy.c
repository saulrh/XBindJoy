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


/* we're going to use a few things here that are specific to the GNU
 * standard libraries; the biggest one is ppoll, which I intend to use
 * eventually so that we can catch signals and release keys before we
 * exit. */
#define _GNU_SOURCE
#include <features.h>

#include <stdlib.h>
#include <libguile.h>
#include <errno.h>
#include <fcntl.h>
#include <ev.h>

#include "xbindjoy.h"
#include "joystick.h"
#include "sender.h"

#define BILLION 1000000000

// /////////////////////////////////////////////////////////////////////////////
// variables

// the structures that hold on to the scheme procedures defined by the user
keymap_t* kmap;
SCM axis_callback;

// how many axes our joystick has
int naxes;

// the file descriptor for our joystick
int jsfd;

// space to store the current joystick axis values
int* axis_vals;

// timing information
double target_timing;
struct timespec last_time;

// /////////////////////////////////////////////////////////////////////////////
// libev callbacks

static void js_callback(EV_P_ ev_io* w, int revents) {
	// read whatever's available for us
	struct js_event e;
	int result = read(jsfd, &e, sizeof(struct js_event));

	// handle the event - either using keys to call out to scheme procedures or updating our axes
	if (e.type == JS_EVENT_BUTTON) {
		handle_and_dispatch_keys(kmap, e);
	}
	else if (e.type == JS_EVENT_AXIS) {
		handle_axis(axis_vals, e);
	}
}

static void timer_callback(EV_P_ ev_timer* w, int revents) {
	// get current time and calculate dt
	struct timespec cur_time;
	clock_gettime(CLOCK_MONOTONIC, &cur_time);
	double dt = (double)(cur_time.tv_sec - last_time.tv_sec)
		+ (double)(cur_time.tv_nsec - last_time.tv_nsec) / BILLION;

	// call out to scheme
	dispatch_axes(axis_vals, naxes, dt, axis_callback);

	/* update time structures so we can calc dt next time */
	last_time.tv_sec = cur_time.tv_sec;
	last_time.tv_nsec = cur_time.tv_nsec;
}

static void sigint_callback(EV_P_ ev_timer* w, int revents) {
	ev_break(loop, EVBREAK_ALL);
}

// /////////////////////////////////////////////////////////////////////////////
// main loop
SCM joystick_loop(SCM jsdevice, SCM keymap_alist, SCM axis_func) {
	// allocate space for and build keymap
	kmap = build_keymap_from_scm_alist(keymap_alist);
	
	// figure out how many axes we have, then allocate space for storage and zero them out
	naxes = scm_to_int(get_joystick_num_axes_wrapper(jsdevice)); /* TODO: FIXME: factor the
	                                                              * get-num-axes functionality out
	                                                              * and call it directly so we
	                                                              * dno't have to stage through
	                                                              * scheme. */
	axis_vals = calloc(naxes, sizeof(int));
	axis_callback = axis_func;
	
	/* open the joystick device */
	/* we're only waiting on one joystick at a time for now, so we're
	 * going to use a single variable and hardcode the struct for the
	 * poll. TODO: handle multiple joysticks. */
	char* jsdevice_c = scm_to_locale_string(jsdevice);
	jsfd = open(jsdevice_c, O_RDONLY);
	free(jsdevice_c);

	// set up event loop
	struct ev_loop* loop = ev_default_loop(0);

	// set up and run watchers
	ev_io js_watcher;
	ev_init(&js_watcher, js_callback);
	ev_io_set(&js_watcher, jsfd, EV_READ);
	ev_io_start(loop, &js_watcher);
    
	ev_timer timer_watcher;
	ev_init(&timer_watcher, timer_callback);
	ev_timer_set(&timer_watcher, 0.0, target_timing);
	ev_timer_start(loop, &timer_watcher);
	clock_gettime(CLOCK_MONOTONIC, &last_time);

	ev_signal sigint_watcher;
	ev_signal_init(&sigint_watcher, sigint_callback, SIGUSR1);
	ev_signal_start(loop, &sigint_watcher);

	// run the event loop and wait for events to start coming in
	ev_run(loop, 0);
	
	// free memory
	free(axis_vals); axis_vals = NULL;
	free(kmap->keys); kmap->keys = NULL;
	free(kmap); kmap = NULL;

	// return success
	return SCM_BOOL_T;
}


// /////////////////////////////////////////////////////////////////////////////
// initialize guile bindings
void init_xbindjoy(void* data, int argc, char** argv) {
	target_timing = 1.0/50.0;

	// the x display to send key events to
	display = XOpenDisplay(getenv("DISPLAY"));

	/* for figuring out joystick parameters */
	scm_c_define_gsubr("device->jsname", 1, 0, 0, get_joystick_name_wrapper);
	scm_c_define_gsubr("get-js-num-axes", 1, 0, 0, get_joystick_num_axes_wrapper);

	/* for sending x events to the screen */
	scm_c_define_gsubr("send-key", 3, 0, 0, send_key_wrapper);
	scm_c_define_gsubr("send-button", 3, 0, 0, send_button_wrapper);
	scm_c_define_gsubr("send-mouserel", 2, 0, 0, send_mouserel_wrapper);
	scm_c_define_gsubr("send-mouseabs", 2, 0, 0, send_mouseabs_wrapper);

	/* and the actual event loop */
	scm_c_define_gsubr("xbindjoy-start", 3, 0, 0, joystick_loop);
}
