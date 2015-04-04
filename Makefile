# makefile. for making things. with make.
# 
#  Copyright 2015 Saul Reynolds-Haertle.
# 
#  This file is part of XBindJoy. 
#  
#  XBindJoy is free software: you can redistribute it and/or modify 
#  it under the terms of the GNU General Public License as published by 
#  the Free Software Foundation, either version 3 of the License, or 
#  (at your option) any later version. 
#  
#  XBindJoy is distributed in the hope that it will be useful, 
#  but WITHOUT ANY WARRANTY; without even the implied warranty of 
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
#  GNU General Public License for more details. 
# 
#  You should have received a copy of the GNU General Public License 
#  along with XBindJoy.  If not, see <http://www.gnu.org/licenses/>. */

# compilation target directories, as usual. here as defaults, ? means they'll be clobbered by
# whatever the user sets.
PREFIX ?= /usr
DESTDIR ?= 

# wildcards for src files, object files, and files for tracking which headers everything depends
# on.
src := $(wildcard src/*.c)
obj := $(src:.c=.o)
dep := $(obj:.o=.d)
mod := saulrh/xbindjoy.scm
solib := libguilexbindjoy.so

# compiler flags. we use pkg-config to get the includes for guile. set the variable dbg if we want
# to compile with debug flags (default to none). ldflags pull in the libraries we need.
CFLAGS := -std=c11 `pkg-config --cflags guile-2.0` -fPIC $(dbg)
LDFLAGS := -lX11 -lXtst -lev `pkg-config --libs guile-2.0`

# our final shared library.
$(solib): $(obj)
	$(CC) -o $@ -shared $^ $(LDFLAGS)

# this is neat: gcc can be used to generate makefile rules which track semantic dependencies on
# header files! no more weird explosions because you changed a header file and now your function
# signatures don't line up.
-include $(dep)
%.d: %.c
	$(CC) -o $@ $(CFLAGS) $< -MM -MT $(@:.d=.o)

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

test: $(solib)
	@LD_LIBRARY_PATH=. guile -L . ./examples/generic.scm


# clean out object files and compilation products
.PHONY: clean
clean:
	rm -f $(obj) libguilexbindjoy.so
	rm -f $(dep)
