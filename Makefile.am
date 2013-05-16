#
# Makefile.am
#
# Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-json.
#
# guile-json is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# guile-json is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-json; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA
#

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

EXTRA_DIST += COPYING.LESSER

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
