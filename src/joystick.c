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

char* get_joystick_name(char* iodev) {

    int jsfd = open(iodev, O_RDONLY);
    size_t buf_size = 1024;
    char* jsname;

    jsname = malloc(buf_size * sizeof(char));
    int rvalue = ioctl(jsfd, JSIOCGNAME(buf_size * sizeof(jsname)), jsname);
    if (rvalue < 0)
        strcpy(jsname, "Unknown");
    if (verbose) 
        printf("get_joystick_name: name = '%s'\n", jsname);

    return jsname;
}
SCM get_joystick_name_wrapper(SCM iodev) {
    char* name = get_joystick_name(scm_to_locale_string(iodev));
    SCM result = scm_from_locale_string(name);
    if (name)
        free(name);
    return result;
}

int get_joystick_num_axes(char* iodev) {
    int jsfd = open(iodev, O_RDONLY);
    int naxes;
    int rvalue = ioctl(jsfd, JSIOCGAXES, &naxes);
    return naxes;
}
SCM get_joystick_num_axes_wrapper(SCM iodev) {
    int naxes = get_joystick_num_axes(scm_to_locale_string(iodev));
    SCM result = scm_from_int(naxes);
    return result;
}


/* ************************************************* */
/* key mapping */

void build_key(SCM pair, bind_key_t* key) {
    SCM action = scm_caar(pair);
    SCM index = scm_cdar(pair);
    SCM proc = scm_cdr(pair);
    
    char* action_c = scm_to_locale_string(scm_symbol_to_string(action));
    key->is_press = strcmp(action_c, "press") == 0;
    free(action_c);

    key->key_index = scm_to_int(index);
    key->function = proc;

    if(verbose)
        printf("build_key: binding %d %s\n", key->key_index, key->is_press?"down":"up");
}

keymap_t* build_keymap_from_scm_alist(SCM kmap_alist) {
    keymap_t* result = malloc(sizeof(keymap_t));
    result->nkeys = scm_to_int(scm_length(kmap_alist));
    result->keys = malloc(result->nkeys * sizeof(bind_key_t));

    if (verbose)
        printf("build_keymap_from_scm_alist: building keymap out of alist of length %d\n", (int)result->nkeys);

    SCM cur = kmap_alist;
    int idx = 0;
    while (!scm_is_null(cur)) {
        build_key(scm_car(cur), result->keys + (idx++));
        cur = scm_cdr(cur);
    }

    return result;
}

int handle_and_dispatch_keys(keymap_t* kmap, struct js_event e) {
    for (size_t i; i < kmap->nkeys; i++) {
        if (kmap->keys[i].is_press == e.value && kmap->keys[i].key_index == e.number) {
            scm_call(kmap->keys[i].function, SCM_UNDEFINED);
        }
    }
}


/* ************************************************* */
/* axis mapping */

int handle_axis(int* axis_vals, struct js_event e) {
    axis_vals[e.number] = e.value;
}

int dispatch_axes(int* axis_vals, size_t naxes, double dt, SCM axis_func) {
    SCM output_alist = SCM_EOL;

    for (size_t i = 0; i < naxes; i++)
        output_alist = scm_acons(scm_from_int(i), scm_from_int(axis_vals[i]), output_alist);
    
    scm_call(axis_func, output_alist, SCM_UNDEFINED);

    if(verbose) {
        printf("Axis values: ");
        for (size_t i = 0; i < naxes; i++)
            printf("%d: %5d  ", (int)i, axis_vals[i]);
        printf("\n");
    }
}
