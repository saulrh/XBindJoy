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


/* data structures for binding axes */
typedef struct {
    int axis_index;
    uint8_t current_value;
    SCM function;
} bind_axis_t;

typedef struct {
    int naxes;
    bind_axis_t* axes;
} axismap_t;


/* functions */
char* get_joystick_name(char* iodev);
SCM get_joystick_name_wrapper(SCM iodev);

keymap_t* build_keymap_from_scm_alist(SCM kmap_alist);
int dispatch_keys(keymap_t* kmap, struct js_event e);

axismap_t* build_axismap_from_scm_alist(SCM amap_alist);
int dispatch_axis(axismap_t* amap, struct js_event e);
