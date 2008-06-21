\name{shared.big.matrix}
\alias{shared.big.matrix}
\title{Create a shared ``big.matrix''.}
\description{
Create a \code{big.matrix} in shared memory.
}
\usage{
shared.big.matrix(nrow, ncol, type = "integer",
                init = 0, dimnames = NULL)
}
\arguments{
  \item{nrow}{number of rows.}
  \item{ncol}{number of columns.}
  \item{type}{the type of the atomic element (\code{"integer"} by default).}
  \item{init}{a scalar value for initializing the matrix; this cannot be a matrix or a vector of values.}
  \item{dimnames}{a \code{\link{dimnames}} attribute: a \code{list} of length 2 giving the row and column names respectively.}
}
\details{
A shared \code{\link{big.matrix}} object is essentially the same as a non-shared 
\code{big.matrix} object except the memory being managed may be shared 
across R sessions.
These object act much like traditional
R matrices.  However, \code{big.matrix} objects protect the user from 
many of the memory-consuming
pitfalls of traditional R matrices and data frames.  

Your system configuration may pose initial problems.  The relevant configuration
file is \code{/etc/sysctl.conf}, and we include a sample file in the package root.
The last two options, \code{kernel.shmmax} and \code{kernel.shmall}, are the ones
that are probably most important.  We also include a little R script, \code{ipc.R},
which can flush out shared memory segments that sometimes persist if you segfault
for some reason, or fail to exit cleanly.
}
\value{
A shared memory \code{\link{big.matrix}} is returned, described, or attached by
these functions. \code{shared.big.matrix()} creates a new matrix in shared memory, while
\code{attach.big.matrix()} creates the R \code{\link{big.matrix}}
object referencing an existing matrix in shared memory as described by
\code{describe()}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, \code{\link{big.matrix}}, or the class documentation \code{\linkS4class{big.matrix}}.}
\examples{
# This example won't run on the Windows version of the package,
# and if you are reading this message you are most certainly using
# a non-Windows version.

# The example is quite silly, as you wouldn't likely do this in a
# single R session.  But if zdescription were passed to another R session
# via SNOW, NetWorkSpaces, or even by a simple file read/write,
# then the attach of the second R process would give access to the
# same object in memory.  Please see the package vignette for real examples.
z <- shared.big.matrix(3, 3, type='integer', init=3)
z[,]
dim(z)
z[1,1] <- 2
z[,]
zdescription <- describe(z)
zdescription
y <- attach.big.matrix(zdescription)
y[,]
y
z
y[1,1] <- -100
y[,]
z[,]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{methods}
