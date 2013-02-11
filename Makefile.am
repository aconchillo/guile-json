SUBDIRS = json

ACLOCAL_AMFLAGS = -I build-aux

PKG_LIST_VERSION=$(shell echo $(PACKAGE_VERSION) | sed "s/\./ /g")

dist-hook:
	$(SHELL) $(top_srcdir)/ChangeLog > $(top_distdir)/ChangeLog
	cp $(top_srcdir)/pkg-list.scm.in $(top_distdir)/pkg-list.scm
	sed -i "s/%VERSION%/$(PKG_LIST_VERSION)/" $(top_distdir)/pkg-list.scm

moddir=$(prefix)/share/guile/site

SOURCES = json.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
