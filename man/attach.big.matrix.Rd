\name{describe, attach.big.matrix}
\alias{describe}
\alias{attach.big.matrix}
\title{The basic shared ``big.matrix'' operations.}
\description{
The \code{attach.big.matrix} creates a new \code{big.matrix} object which
references previously allocated shared memory or file-backed matrices.

The \code{describe} function returns the description needed by the 
\code{attach.big.matrix} function so that a newly created \code{big.matrix}
instance can reference a previously allocated shared memory instance of
type \code{big.matrix}.
}
\usage{
describe(x)
attach.big.matrix(obj, backingpath='')
}
\arguments{
  \item{x}{a shared \code{\link{big.matrix}}.}
  \item{obj}{an object as returned by \code{describe()} or, optionally, the filename of the descriptor for a filebacked matrix, assumed to be in the directory specified by the \code{backingpath} (if one is provided).}
  \item{backingpath}{the path where the descriptor and/or filebacking can be found.}
}
\details{
After a shared (possible file-backed)
\code{big.matrix} instance is created, the \code{describe} function
can be called to retrieve the keys which refer to the shared memory locations
which the instance manages.  These keys are then used for a new shared
\code{big.matrix} instance to refer to the same memory across R sessions.

A descriptor file is automatically created when a new filebacked \code{big.matrix}
is created, so in this case, the \code{describe()} is unnecessary (but may still
be helpful).
}
\value{
\code{describe} returns a list of shared memory keys and other necessary information.

\code{attach.big.matrix} return a new instance of type \code{big.matrix} which
refers to the shared memory described by the \code{describe} function.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, \code{\link{big.matrix}}, or the class documentation \code{\linkS4class{big.matrix}}.}
\examples{
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
