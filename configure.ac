#
# configure.ac
#
# Copyright (C) 2013-2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-json.
#
# guile-json is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License and
# the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# guile-json is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License and the GNU Lesser General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# and the GNU Lesser General Public License along with guile-json;
# if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
#

AC_INIT([guile-json], [0.6.0], [aconchillo@gmail.com])
AC_CONFIG_SRCDIR(json.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

GUILE_PROGS

AC_CONFIG_FILES([Makefile json/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT

# configure.ac ends here
