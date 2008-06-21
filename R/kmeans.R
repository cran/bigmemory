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


# The worker.km() function needs to attach the data matrix in shared memory,
# if applicable, and will only read the value; thus, we ignore locks.

worker.km <- function(i, desc) {
  require(bigmemory)

  if (!is.big.matrix(desc[[1]])) {
    x <- attach.big.matrix(desc[[1]])             # BIG * m shared description
  } else {
    x <- desc[[1]]                                # BIG * m big.matrix
  } 

  m <- ncol(x)
  n <- nrow(x)
  k <- desc[[2]]
  centers <- desc[[3]]
  max.iters <- desc[[4]]
  mattype <- desc[[5]]

  # Created locally:

  cent <-  big.matrix(k, m, type='double', init=0)        # k * m
  ss <-    big.matrix(k, 1, type='double', init=0)        # k * 1
  clust <- big.matrix(nrow(x), 1, type='integer', init=0) # BIG * 1 (holds integers)
  clustsizes <- big.matrix(k, 1, type='double', init=0)  # k * 1 

  if (is.matrix(centers)) {
    cent[,] <- centers
  } else {
    cent[,] <- x[sample(1:n, k),]
  }

  iters = .Call("Ckmeans2main", as.integer(mattype),                   
                x@address, cent@address, ss@address,
                clust@address, clustsizes@address,
                as.double(n), as.integer(k), as.double(m), as.integer(max.iters))

  ans <- list(cluster=clust[,], centers=cent[,], withinss=ss[,], size=clustsizes[,],
              iters=iters)
  class(ans) <- "kmeans"

  return(ans)

}

kmeans.big.matrix <- function(x, centers, iter.max = 10, nstart = 1,
                      algorithm = "MacQueen", tol=1e-8, parallel = NA,
                      nwssleigh = NULL) {

  # x a big.matrix possibly in shared memory, massive rows by m columns.
  # - If centers is an integer, choose random points to start with.
  # - If nstart>1 and centers is a matrix, use it only to get k.
  # - tol is not yet used.
  # parallel = "nws" (or "snow", eventually... without snow being distributed
  #            for Windows on CRAN, it caused problems with the packaging).
  # Note: if using NWS, the sleigh had better be restricted to this machine,
  # because the shared big.matrix objects are not distributed.

  ########################################################################

  library(bigmemory)
  if (is.big.matrix(x)) {

    if (!is.matrix(centers)) {
      if (is.numeric(centers) && length(centers)==1) {
        k <- centers
        centers <- NA
      } else stop("centers must be a matrix of centers or number of clusters")
    } else {
      k <- nrow(centers)
      if (nstart>1) {
        warning("Random starting points will be used, not the centers you provided.\n")
        centers <- NA
      }
    }

    desc <- vector(5, mode="list")

    # If the matrix is a shared object, then get the description, otherwise pass the matrix pointer.
    if (is.shared(x)) {
      desc[[1]] <- describe(x)
    } else {
      desc[[1]] <- x
    }

    desc[[2]] <- k
    desc[[3]] <- centers
    desc[[4]] <- iter.max
    if (typeof(x)=="char") desc[[5]] <- 1
    if (typeof(x)=="short") desc[[5]] <- 2
    if (typeof(x)=="integer") desc[[5]] <- 4
    if (typeof(x)=="double") desc[[5]] <- 8

  } else stop("x needs to be a big.matrix.\n")

  if (is.shared(x) && parallel=="nws" && nstart>1 && !is.null(nwssleigh)) {

    cat("Running multiple starts in parallel using NetWorkSpaces.\n")
    ans <- eachElem(nwssleigh, worker.km, elementArgs=rep(1,nstart), fixedArgs=list(desc))
    best <- 1
    if (length(ans)>1) {
      for (i in 2:length(ans)) {
        if (sum(ans[[i]]$withinss) < sum(ans[[best]]$withinss))
          best <- i
      }
    }
    ans <- ans[[best]]

  } else { # proceed iteratively

    ans <- worker.km(1, desc)
    if (nstart>1) {
      for (i in 2:nstart) {
        temp <- worker.km(i, desc)
        if (sum(temp$withinss) < sum(ans$withinss)) ans <- temp
      }
    }

  }

  ####################################
  ####################################

  if (ans$iters>=iter.max) 
    warning("kmeans.big.matrix did not converge in ", iter.max, " iterations.")
  ans$iters <- NULL

  return(ans)

}
