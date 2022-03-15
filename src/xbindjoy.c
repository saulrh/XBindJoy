/* xbindjoy.c: event loop and some basic functions for the xbindjoy guile module
 *
 * Copyright 2015 Saul Reynolds-Haertle.
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

int verbose;
Display* display;

// the file descriptor for our joystick
int jsfd;

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
		handle_and_dispatch_button(e);
	}
	else if (e.type == JS_EVENT_AXIS) {
		handle_axis_event(e);
	}
}

static void timer_callback(EV_P_ ev_timer* w, int revents) {
	// get current time and calculate dt
	struct timespec cur_time;
	clock_gettime(CLOCK_MONOTONIC, &cur_time);
	double dt = (double)(cur_time.tv_sec - last_time.tv_sec)
		+ (double)(cur_time.tv_nsec - last_time.tv_nsec) / BILLION;

	// call out to scheme
	dispatch_axis_bindings(dt);
	
	/* update time structures so we can calc dt next time */
	last_time.tv_sec = cur_time.tv_sec;
	last_time.tv_nsec = cur_time.tv_nsec;
}

static void sigint_callback(EV_P_ ev_signal* w, int revents) {
	ev_break(loop, EVBREAK_ALL);
}


// /////////////////////////////////////////////////////////////////////////////
// main loop
SCM joystick_loop(SCM jsdevice) {
	/* open the joystick device */
	/* we're only waiting on one joystick at a time for now, so we're going to use a single
	 * variable. TODO: handle multiple joysticks. */
	char* jsdevice_c = scm_to_locale_string(jsdevice);
	jsfd = open(jsdevice_c, O_RDONLY);

	// check to make sure our joystick is real
	if (jsfd < 0) {
		printf("Could not open device %s: %s\n", jsdevice_c, strerror(errno));
		return SCM_BOOL_F;
	}

	// clean up the filename string.
	free(jsdevice_c);
	
	// set up event loop
	struct ev_loop* loop = ev_default_loop(0);

	// set up and run watchers
	
	// file watcher waiting for new events from the joystick. this is where joystick data gets into
	// xbindjoy and where procedures bound to buttons are called from.
	ev_io js_watcher;
	ev_init(&js_watcher, js_callback);
	ev_io_set(&js_watcher, jsfd, EV_READ);
	ev_io_start(loop, &js_watcher);
    
	// timer watcher that pings at a regular (30hz-ish) rate. this is where procedures bound to
	// axes are called from.
	ev_timer timer_watcher;
	ev_init(&timer_watcher, timer_callback);
	ev_timer_set(&timer_watcher, 0.0, target_timing);
	ev_timer_start(loop, &timer_watcher);
	clock_gettime(CLOCK_MONOTONIC, &last_time);

	// signal watcher. safely clean up and exit on SIGINT.
	ev_signal sigint_watcher;
	ev_signal_init(&sigint_watcher, sigint_callback, SIGINT);
	ev_signal_start(loop, &sigint_watcher);

	// run the event loop and wait for events to start coming in
	ev_run(loop, 0);

	// close file descriptors.
	close(jsfd);
	
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
	scm_c_define_gsubr("get-js-num-buttons", 1, 0, 0, get_joystick_num_buttons_wrapper);

	/* for sending x events to the screen */
	scm_c_define_gsubr("send-key", 3, 0, 0, send_key_wrapper);
	scm_c_define_gsubr("send-mbutton", 3, 0, 0, send_button_wrapper);
	scm_c_define_gsubr("send-mouserel", 2, 0, 0, send_mouserel_wrapper);
	scm_c_define_gsubr("send-mouseabs", 2, 0, 0, send_mouseabs_wrapper);

	/* finally, the functions for actually adding your own functionality */
	scm_c_define_gsubr("bind-button->proc", 2, 0, 0, add_button_binding_wrapper);
	scm_c_define_gsubr("bind-axis->proc", 1, 0, 0, add_axis_binding_wrapper);

	/* and the event loop */
	scm_c_define_gsubr("init-xbindjoy", 2, 0, 0, init_bindings_wrapper);
	scm_c_define_gsubr("xbindjoy-start", 1, 0, 0, joystick_loop);
}
