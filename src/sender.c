/* sender.c: functions for sending mouse and keyboard events from guile
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

#include <X11/X.h>
#include <X11/keysym.h>
#include <X11/extensions/XTest.h>
#include <libguile.h>
#include <ctype.h>
#include <math.h>

#include "xbindjoy.h"
#include "sender.h"

/* ************************************************* */
/* code that actually does things */
int send_key(KeyCode xkeycode, int is_press, int delayms) {
	int result = XTestFakeKeyEvent(display, (unsigned int)xkeycode, is_press, delayms);
	XFlush(display);
	return result;
}

int send_button(int xbuttoncode, int is_press, int delayms) {
	int result = XTestFakeButtonEvent(display, (unsigned int)xbuttoncode, is_press, delayms);
	XFlush(display);
	return result;
}

int send_mouserel(int x, int y) {
	int result = XTestFakeRelativeMotionEvent(display, x, y, 0); 
	XFlush(display);
	return result;
}

int send_mouseabs(int x, int y) {
	int result = XTestFakeMotionEvent(display, DefaultScreen(display), x, y, 0); 
	XFlush(display);
	return result;
}


/* ************************************************* */
/* functions for interfacing with guile */

/* takes a guile string or guile symbol for a key and figures out the
 * appropriate keycode */
KeyCode xkey_scm_to_keycode(SCM xkey) {
	KeyCode keycode;
	KeySym keysym;
	char* keyname;
	if (scm_symbol_p(xkey)) keyname = scm_to_locale_string(scm_symbol_to_string(xkey));
	else keyname = scm_to_locale_string(xkey);

	/* parse out the string into a keycode or a keysym */
	if (strlen (keyname) > 2 && keyname[0] == 'c' && keyname[1] == ':' && isdigit(keyname[2])) {
		keycode = strtol (keyname+2, (char **) NULL, 0);
	}
	else //regular key
	{
		keysym = XStringToKeysym (keyname);
		if (keysym == 0){
			printf("No keysym for key: %s\n", keyname);
			free(keyname);
			return -1;
		}
		keycode = XKeysymToKeycode(display, keysym);
	}

	free(keyname);
	return keycode;
}



SCM send_key_wrapper(SCM action, SCM xkey, SCM delay) {
	KeyCode keycode;
	keycode = xkey_scm_to_keycode(xkey);
	if (keycode == -1)          /* couldn't find an appropriate keysym */
		/* TODO: perhaps throw an out-of-range error here? */
		return SCM_BOOL_F;

	int delayms;
	if (delay == SCM_UNDEFINED) delayms = 0;
	else delayms = scm_to_int(delay);
    
	/* actually send the press, return result */
	int result;
	char* action_string = scm_to_locale_string(scm_symbol_to_string(action));
	result = send_key(keycode,
	                  strcmp(action_string, "press") == 0,
	                  delayms);
	free(action_string);

	if (result) return SCM_BOOL_T;
	return SCM_BOOL_F;
}
SCM send_button_wrapper(SCM action, SCM xbutton, SCM delay) {
	int delayms;
	if (delay == SCM_UNDEFINED) delayms = 0;
	else delayms = scm_to_int(delay);

	int result;
	char* action_string = scm_to_locale_string(scm_symbol_to_string(action));
	result = send_button(scm_to_int(xbutton),
	                     strcmp(action_string, "press") == 0,
	                     delayms);
	free(action_string);
    
	if (result) return SCM_BOOL_T;
	return SCM_BOOL_F;
}

SCM send_mouserel_wrapper(SCM x, SCM y) {
	/* TODO: apply dithering instead of simple rounding */
	int result = send_mouserel((int)round(scm_to_double(x)), (int)round(scm_to_double(y)));
	if (result) return SCM_BOOL_T;
	else return SCM_BOOL_F;
}

SCM send_mouseabs_wrapper(SCM x, SCM y) {
	int result = send_mouseabs((int)round(scm_to_double(x)), (int)round(scm_to_double(y)));
	if (result) return SCM_BOOL_T;
	else return SCM_BOOL_F;
}
