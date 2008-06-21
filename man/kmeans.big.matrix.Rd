\name{kmeans.big.matrix}
\alias{kmeans.big.matrix}
\title{bigmemory's memory-efficient k-means}
\description{
k-means cluster analysis without the memory overhead, and possibly
in parallel using shared memory.
}
\usage{
kmeans.big.matrix(x, centers, iter.max = 10, nstart = 1,
                  algorithm = "MacQueen", tol = 1e-8,
                  parallel = NA, nwssleigh = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{\link{big.matrix}} object.}
  \item{centers}{a scalar denoting the number of clusters, or for k clusters, a k by \code{ncol(x)} matrix.}
  \item{iter.max}{the maximum number of iterations.}
  \item{nstart}{number of random starts, to be done in parallel if possible.}
  \item{algorithm}{only MacQueen's algorithm has been implemented at this point.}
  \item{tol}{the convergence tolerance, not used at this point.}
  \item{parallel}{\code{"nws"} for NetWorkSpaces; we couldn't include \code{"snow"} because CRAN doesn't distribute it for Windows and so we ran into R CMD check problems.}
  \item{nwssleigh}{the NWS sleigh (which should be limited to this workstation and could have multiple processors).}
}
\details{
The real benefit is the lack of memory overhead compared to the standard
\code{\link{kmeans}} function.  With a \code{big.matrix},
\code{kmeans.big.matrix()} requires essentially no extra
memory (beyond the data, other than recording the cluster memberships), 
whereas \code{kmeans()} makes at least two extra copies of the data.  In fact,
\code{kmeans()} is even worse if multiple starts (\code{nstart>1}) are used.
It's a little surprising, as you can see from the examples, below.

If \code{nstart>1} and you are using \code{kmeans.big.matrix()} in parallel, a vector of cluster memberships 
will need to be stored for each random starting point, which could be 
memory-intensive for large data.  This isn't a problem if you use are running
the multiple starts sequentially.

Unless you have a really big data set (where a single run of \code{\link{kmeans}} not only burns memory but takes more than a few seconds), using NWS for parallel computing of multiple random starts is unlikely to be much faster than running iteratively.
}
\note{
You should make sure you have used NetWorkSpaces successfully with a simple
example from their help pages before trying \code{kmeans.big.matrix()}.  Also, bear in mind that
the shared-memory parallel algorithm is only working on the local machine (not sharing
resources of a cluster of machines).  It doesn't make sense to allocate more
clients than the number of available cores.
}
\value{
An object of class \texttt{kmeans}, just as produced by \code{\link{kmeans}}.
}
\author{John W. Emerson and Michael J. Kane }
\seealso{\code{\link{big.matrix}}}
\examples{
# Simple example (with one processor, because we don't want to require the
# installation of package nws here:

  x <- big.matrix(100000, 3, init=0, type="double")
  x[seq(1,100000,by=2),] <- rnorm(150000)
  x[seq(2,100000,by=2),] <- rnorm(150000, 5, 1)
  head(x)
  ans <- kmeans.big.matrix(x, 2, nstart=5)    # Sequential multiple starts.

  # To use NWS, try something like the following:
  \dontrun{
    library(nws)
    s <- sleigh(nwsHost='yourhostname.xxx.yyy.zzz', workerCount=2)
    ans <- kmeans.big.matrix(x, 2, nstart=5, parallel='nws', nwssleigh=s)
    stopSleigh(s)
  }

  # Both the following are run iteratively, but with less memory overhead using
  # kmeans.big.matrix.  Note that this first gc() doesn't reflect the C++
  # memory usage for the big.matrix, but the maximum memory used is about
  # 35 MB after kmeans.big.matrix().
  gc(reset=TRUE)
  time.new <- system.time(print(kmeans.big.matrix(x, 2, nstart=5)$centers))
  gc()
  y <- x[,]
  rm(x)
  # In contrast, the regular kmeans() really burns through the memory:
  gc(reset=TRUE)
  time.old <- system.time(print(kmeans(y, 2, nstart=5)$centers))
  gc()
  # The new kmeans() centers should match the old kmeans() centers, without
  # the memory overhead running more quickly; it isn't a problem with the guts of the 
  # kmeans() implementation (the algorithm is in C, well-implemented), but in
  # the traditional C/R interface and R code managing the objects and nstart:
  time.new
  time.old
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
