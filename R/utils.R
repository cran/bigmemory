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


setGeneric('binit', function(x, cols, breaks=10)
  standardGeneric('binit'))

setMethod('binit', signature(x='big.matrix'),
  function(x, cols, breaks=10)
  {
    if (!is.big.matrix(x)) stop("Error in binit: must be a big.matrix.")
    if (is.null(cols)) stop("Error in binit: must provide 1 or 2 columns.")
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    if (length(cols)<1 || length(cols)>2) {
      stop("Error in binit: only 1 or 2 columns is supported.")
    }
    if (!is.list(breaks) && length(breaks)==1) {
      usebreaks <- breaks
      if (length(cols)==1) {
        breaks <- c(colmin(x, cols, na.rm=TRUE), colmax(x, cols, na.rm=TRUE), usebreaks)
      }
      if (length(cols)==2) {
        mins <- colmin(x, cols, na.rm=TRUE)
        maxs <- colmax(x, cols, na.rm=TRUE)
        breaks <- list(c(mins[1], maxs[1], usebreaks),
                       c(mins[2], maxs[2], usebreaks))
      }
    }
    if (!is.list(breaks) && length(breaks)!=3) stop("Incorrect breaks in binit().")
    if (is.list(breaks) & (length(breaks)!=2 ||
                           length(breaks[[1]])!=3 || length(breaks[[2]])!=3))
      stop("Error in binit: breaks problem.")
    thistype = .Call("CGetType", x@address)
    if (is.list(breaks)) {
      ret = .Call("CBinItmain2", as.integer(thistype), x@address,
        as.double(cols), as.double(breaks[[1]]), as.double(breaks[[2]]))
      ret <- matrix(ret, breaks[[1]][3], breaks[[2]][3])
      rb <- seq(breaks[[1]][1], breaks[[1]][2], length.out=breaks[[1]][3]+1)
      rnames <- (rb[-length(rb)] + rb[-1]) / 2
      cb <- seq(breaks[[2]][1], breaks[[2]][2], length.out=breaks[[2]][3]+1)
      cnames <- (cb[-length(cb)] + cb[-1]) / 2
      ret <- list(counts=ret, rowcenters=rnames, colcenters=cnames,
                  rowbreaks=rb, colbreaks=cb)
    } else {
      ret = .Call("CBinItmain1", as.integer(thistype), x@address,
        as.double(cols), as.double(breaks))
      b <- seq(breaks[1], breaks[2], length.out=breaks[3]+1)
      rnames <- (b[-length(b)] + b[-1]) / 2
      ret <- list(counts=ret, centers=rnames, breaks=b)
    }

    return(ret)
  })


