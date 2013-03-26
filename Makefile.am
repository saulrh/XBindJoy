ACLOCAL_AMFLAGS = -I m4


AM_CPPFLAGS = -I`pkg-config guile-2.0 --cflags`
AM_LDFLAGS = `pkg-config --libs guile-2.0`

# SUBDIRS = .
# dist_doc_DATA = README
bin_PROGRAMS = xbindjoy

xbindjoy_SOURCES = src/xbindjoy.c

