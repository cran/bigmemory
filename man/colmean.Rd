\name{colmean, colmin, colmax, colrange, colvar, colsd, colprod, colsum, colna}
\alias{colmean}
\alias{colmin}
\alias{colmax}
\alias{colrange}
\alias{colvar}
\alias{colsd}
\alias{colprod}
\alias{colsum}
\alias{colna}
\title{Basic statistics for ``big.matrix'' objects.}
\description{
These functions operate on columns of the \code{\link{big.matrix}}.
}
\usage{
colmean(x, cols, na.rm)
colmin(x, cols, na.rm)
colmax(x, cols, na.rm)
colrange(x, cols, na.rm)
colvar(x, cols, na.rm)
colsd(x, cols, na.rm)
colsum(x, cols, na.rm)
colprod(x, cols, na.rm)
colna(x, cols)
}
\arguments{
  \item{x}{a \code{\link{big.matrix}}.}
  \item{cols}{a scalar or vector of column(s) to be summarized.}
  \item{na.rm}{if \code{TRUE}, remove \code{NA} values before summarizing.}
}
\details{
These functions essentially apply summary functions to each column (or
each specified column) of the \code{\link{big.matrix}} in turn.
}
\value{
For \code{colrange}, a matrix with two columns and \code{length(cols)}
rows; column 1 contains the minimum, and column 2 contains the maximum for
that column.  The other functions return vectors of length \code{length(cols)}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{
x <- as.big.matrix(matrix(sample(1:10, 20, replace=TRUE), 5, 4))
colnames(x) <- c("a", "b", "c", "d")
x[,]
mean(x)
colmean(x)
colmin(x)
colmin(x, 1)
colmax(x)
colmax(x, "b")
colsd(x)
colrange(x)
range(x)
colsum(x)
colprod(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
