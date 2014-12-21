

CFLAGS=-std=c11
LIBS=-lX11 -lXtst


all:
	gcc `pkg-config --cflags guile-2.0` $(CFLAGS) -shared -o libxbindjoy.so -fPIC src/*.c $(LIBS)



# to install, copy resulting .so to /usr/lib or /usr/local/lib. to test it, add it to
# LD_LIBRARY_PATH
