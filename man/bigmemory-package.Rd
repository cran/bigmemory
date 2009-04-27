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
Version: \tab 3.6\cr
Date: \tab 2009-04-27\cr
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

Note that \code{options(bigmemory.typecast.warning)} is available and can
be set to avoid annoying warnings that might occur if, for example you
assign R objects (typically type double) to char, short, or integer
\code{big.matrix} objects.

}
\author{
John W. Emerson and Michael J. Kane

Maintainer: Jay Emerson <john.emerson@yale.edu>
}
\keyword{package}
\references{See \url{http://www.stat.yale.edu/~jay/bigmemory}.}
\seealso{For example,
\code{\link{big.matrix}}, \code{\link{mwhich}}, \code{\link{colmean}}, \code{\link[biglm:biglm-package]{biglm}}
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
