% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bigmemory.R
\name{describe,big.matrix-method}
\alias{describe,big.matrix-method}
\alias{attach.big.matrix}
\alias{describe}
\alias{attach.resource}
\title{The basic ``big.matrix'' operations for sharing and re-attaching.}
\usage{
\S4method{describe}{big.matrix}(x)

attach.big.matrix(obj, ...)
}
\arguments{
\item{x}{a \code{\link{big.matrix}} object}

\item{obj}{an object as returned by \code{describe()} or, optionally,
the filename of the descriptor for a filebacked matrix, assumed to be in
the directory specified by the \code{path} (if one is provided)}

\item{...}{possibly \code{path} which gives the path where the descriptor
and/or filebacking can be found}
}
\value{
\code{describe} returns a list of of the information needed to attach to
a \code{big.matrix} object.

\code{attach.big.matrix} return a new instance of type \code{big.matrix}
corresponding to a shared-memory or file-backed \code{big.matrix}.
}
\description{
The \code{describe} function returns the information needed by
\code{attach.big.matrix} to reference a shared or file-backed
\code{big.matrix} object.
The \code{attach.big.matrix} and \code{attach.resource} functions create a
new \code{big.matrix} object based on the descriptor information referencing
previously allocated shared-memory or file-backed matrices.
}
\details{
The \code{describe} function returns a list of the information needed to
attach to a \code{big.matrix} object.
A descriptor file is automatically created when a new filebacked
\code{big.matrix} is created.
}
\examples{
# The example is quite silly, as you wouldn't likely do this in a
# single R session.  But if zdescription were passed to another R session
# via SNOW, foreach, or even by a simple file read/write,
# then the attach of the second R process would give access to the
# same object in memory.  Please see the package vignette for real examples.

z <- big.matrix(3, 3, type='integer', init=3)
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
zz <- attach.resource(zdescription)
zz[1,1] <- -100
y[,]
z[,]
}
\seealso{
\code{\link{bigmemory}}, \code{\link{big.matrix}}, or the class
documentation \code{\linkS4class{big.matrix}}.
}
\author{
Michael J. Kane and John W. Emerson
\email{bigmemoryauthors@gmail.com}
}
\keyword{classes}
\keyword{methods}
