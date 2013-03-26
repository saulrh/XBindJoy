/* sender.h: functions for sending mouse and keyboard events from guile
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

int send_key(KeyCode xkeycode, int is_down);
int send_button(int xbuttoncode, int is_down);
int send_mouserel(int x, int y);
int send_mouseabs(int x, int y);


SCM send_key_wrapper(SCM xkey);
SCM send_button_wrapper(SCM xbuttoncode);
SCM send_mouserel_wrapper(SCM x, SCM y);
SCM send_mouseabs_wrapper(SCM x, SCM y);
