# /*
#  *  bigmemory: an R package for managing massive matrices using C,
#  *  with support for shared memory.
#  *
#  *  Copyright (C) 2008 John W. Emerson and Michael J. Kane
#  *
#  *  This file is part of bigmemory.
#  *
#  *  bigmemory is free software; you can redistribute it and/or modify
#  *  it under the terms of the GNU Lesser General Public License as published
#  *  by the Free Software Foundation; either version 3 of the License, or
#  *  (at your option) any later version.
#  *
#  *  This program is distributed in the hope that it will be useful,
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  *  GNU Lesser General Public License for more details.
#  *
#  *  You should have received a copy of the GNU Lesser General Public License
#  *  along with this program; if not, a copy is available at
#  *  http://www.r-project.org/Licenses/
#  */

# ###########
# FILE: zzz.R
#
# We make use of two global options, here: rlock.enabled is used for
# subtle reasons, having to do most likely with an assignment having
# a nested mwhich, such as
#
# x[mwhich(...),] <- something
#
# Here, a write lock is obtained, but then which.bm will try to get
# a read lock (and fail).  We use the rlock.enabled to avoid this conflict. 
# There are probably other similar examples.

.onLoad <- function(libname, pkgname) {
    library.dynam("bigmemory", pkgname, libname);
    options(rlock.enabled=TRUE)
    options(bm.print.warning=TRUE)
}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(rlock.enabled=NULL)
    options(bm.print.warning=NULL)
}
