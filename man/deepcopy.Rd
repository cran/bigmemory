\name{deepcopy}
\alias{deepcopy}
\title{Produces a physical copy of a ``big.matrix''}
\description{
This is needed to make a duplicate of a \code{big.matrix}; currently,
it creates a non-shared \code{big.matrix} regardless of the nature
of the argument, and this will be changed to support additional options.
}
\usage{
deepcopy(x, type = NULL, separated = NULL, shared = NULL, backingfile = NULL,
         backingpath = NULL, preserve = TRUE)
}
\arguments{
  \item{x}{a \code{\link{big.matrix}}.}
  \item{type}{preferably specified, \code{"integer"} for example.}
  \item{separated}{use separated column organization of the data instead of column-major organization.}
\item{shared}{if \code{TRUE}, load the object into shared memory.}
  \item{backingfile}{the root name for the file(s) for the cache of \code{x}.}
  \item{backingpath}{the path to the directory containing the file backing cache.}
  \item{preserve}{if this is a filebacked \code{big.matrix}, it is preserved, by default, even after the end of the R session unless this option is set to \code{FALSE}.}
}
\details{
This is needed to make a duplicate of a \code{\link{big.matrix}}, because traditional
R syntax would only copy the R object (the pointer to the \code{\link{big.matrix}}
rather than the \code{\link{big.matrix}} itself).

This function needs to be revised to support shared and file-backed objects.  Not
a big deal, and definitely on our todo list.
}
\value{
a \code{\link{big.matrix}}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{
x <- as.big.matrix(matrix(1:30, 10, 3))
y <- deepcopy(x)
x
y
head(x)
head(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
