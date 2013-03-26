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
