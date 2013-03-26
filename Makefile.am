ACLOCAL_AMFLAGS = -I m4


AM_CPPFLAGS = -I`pkg-config guile-2.0 --cflags`
AM_LDFLAGS = `pkg-config --libs guile-2.0`

# SUBDIRS = .
# dist_doc_DATA = README
lib_LTLIBRARIES = libguile-xbindjoy.la

libguile_xbindjoy_la_SOURCES = src/xbindjoy.c src/joystick.c
libguile_xbindjoy_la_LTFLAGS = -version-info 0:0:0

