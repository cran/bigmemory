\name{deepcopy}
\alias{deepcopy}
\title{Produces a physical copy of a ``big.matrix''}
\description{
This is needed to make a duplicate of a \code{big.matrix}, with the
new copy optionally filebacked.
}
\usage{
deepcopy(x, cols = NULL, type = NULL, separated = NULL,
  backingfile = NULL, backingpath = NULL, descriptorfile = NULL,
  shared=TRUE)
}
\arguments{
  \item{x}{a \code{\link{big.matrix}}.}
  \item{cols}{possible subset of columns for the deepcopy; could be numeric, named, or logical.}
  \item{type}{preferably specified, \code{"integer"} for example.}
  \item{separated}{use separated column organization of the data instead of column-major organization; use with caution if the number of columns is large.}
  \item{backingfile}{the root name for the file(s) for the cache of \code{x}.}
  \item{backingpath}{the path to the directory containing the file-backing cache.}
  \item{descriptorfile}{we recommend specifying this for file-backing.}
  \item{shared}{\code{TRUE} by default, and always \code{TRUE} if the \code{big.matrix} is file-backed.  For a non-filebacked \code{big.matrix}, \code{shared=FALSE} uses non-shared memory, which can be more stable for large (say, >50\% of RAM) objects.  Shared memory allocation can sometimes fail in such cases due to exhausted shared-memory resources in the system.}
}
\details{
This is needed to make a duplicate of a \code{\link{big.matrix}}, because traditional
\R syntax would only copy the \R object (the pointer to the \code{\link{big.matrix}}
rather than the \code{\link{big.matrix}} itself).
It can also make a copy of only a subset of columns.
}
\value{
a \code{\link{big.matrix}}.
}
%\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{

x <- as.big.matrix(matrix(1:30, 10, 3))
y <- deepcopy(x, -1)    # Don't include the first column.
x
y
head(x)
head(y)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
