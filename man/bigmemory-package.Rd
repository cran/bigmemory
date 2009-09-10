\name{bigmemory-package}
\alias{bigmemory-package}
\alias{bigmemory}
\docType{package}
\title{
bigmemory: massive matrices in (possibly shared) memory.
}
\description{
\pkg{bigmemory} implements massive matricies in C++ 
and supports their basic manipulation and exploration.  
Access to and manipulation
of a \code{big.matrix} object is exposed in R by an S4 
class whose interface is simlar to an R \code{matrix}.
}
\details{
\tabular{ll}{
Package: \tab bigmemory\cr
Type: \tab Package\cr
Version: \tab 3.11\cr
Date: \tab 2009-09-10\cr
License: \tab LGPL-3\cr
}

Multi-gigabyte data sets challenge and frustrate R
users even on well-equipped hardware.
C/C++ and Fortran programming can be helpful, but
are cumbersome for interactive data analysis and
lack the flexibility and power of R's rich statistical
programming environment.  The package \code{bigmemory}
bridges this gap, implementing massive matrices 
and supporting their basic
manipulation and exploration.  It is ideal for problems
involving the analysis in R of manageable subsets of the data,
or when an analysis is conducted mostly in C++. 
The data structures may be allocated to shared memory with
transparent read and write locking, allowing separate 
processes on the same computer to share access to a single copy of the
data set.  The data structures may also be file-backed, allowing users
to more easily manage and analyze data sets larger than available RAM.
These features of \code{bigmemory} open the door for powerful and
memory-efficient parallel analyses and data mining of massive data sets.

This package is still actively developed, although the 3.X tree has
essentially been frozen.  The upcoming 4.0 release (Fall 2009) will
include some important changes (see below).  Please send us an email
letting us know you are trying the package, and we'll keep you
abreast on updates.

Note that \code{options(bigmemory.typecast.warning)} is available and can
be set to avoid annoying warnings that might occur if, for example you
assign R objects (typically type double) to char, short, or integer
\code{big.matrix} objects.

Earlier versions of bigmemory included a function for k-means clustering.
This has been temporarily removed and will be located in a new package,
biganalytics (or perhaps bigmemoryanalytics0 in the Fall of 2009.
At the same time, biglm.big.matrix and bigglm.big.matrix will be
relocated to the same new package and removed from bigmemory itself.

The 3.X and earlier versions support a limited number of columns
(due to mutex limitations), roughly 50,000 on a typical Linux system.
This restriction will be removed in versions 4.0 and beyond, when the
mutex will be removed from bigmemory and made available in a new package,
synchronicity.

There were row limitations (due to a bug that has now been fixed) in versions
3.8 and earlier of roughly 1 billion, but this has been fixed in versions
3.82 and later.  We apologize for the inconvenience, and appreciate
and and all feedback.  - Jay and Mike
}
\author{
John W. Emerson and Michael J. Kane

Maintainer: Jay Emerson <john.emerson@yale.edu>
}
\keyword{package}
\references{See \url{http://www.stat.yale.edu/~jay/bigmemory}.}
\seealso{For example,
\code{\link{big.matrix}}, \code{\link{mwhich}}, \code{\link{colmean}}
}
\examples{
# Our examples are all trivial in size, rather than burning huge amounts
# of memory simply to demonstrate the package functionality.

x <- big.matrix(5, 2, type="integer", init=0)
colnames(x)=c("alpha", "beta")
x
x[,]
x[,1] <- 1:5
x[,]
mean(x)
colmean(x)
summary(x)
}
