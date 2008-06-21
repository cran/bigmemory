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


# This function is used to match up a vector of column names to the
# entire set of column names, providing the proper column indices.
# The name choice was based on the phrase "multiple map" though
# perhaps we should have made a different choice. - JE 1/16/08

# Sorry, the name choice wasn't great.

mmap = function(x, y) {
  if (is.null(x)) return(NULL)
  ans <- match(x, y) #y %in% x
  if (length(ans) == length(x)) return(ans)
  else stop("Couldn't find a match to one of the arguments.")
}

