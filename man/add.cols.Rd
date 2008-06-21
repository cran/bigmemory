\name{add.cols, rm.cols}
\alias{add.cols}
\alias{rm.cols}
\title{Add and remove columns of a ``big.matrix''.}
\description{
Add and remove columns of a \code{\link{big.matrix}} (only valid for non-shared matrices).
}
\usage{
add.cols(x, ncol = 1, init = 0, column.names = NULL)
rm.cols(x, remove.columns)
}
\arguments{
  \item{x}{a \code{big.matrix}.}
  \item{ncol}{the number of columns to add.}
  \item{remove.columns}{a vector of integer indices or variable names to remove.}
  \item{init}{a scalar value for initializing the column(s); this may not be a vector or a matrix of values.}
  \item{column.names}{a vector of \code{ncols} names (optional).}
}
\details{
This function adds and removes columns of a \code{big.matrix}
with no memory overhead.  Note that these operations are not
permitted if the matrix is in shared memory (if \code{is.shared(x)} is
\code{TRUE}).
}
\value{
None; the \code{\link{big.matrix}}, \code{x}, is actually modified.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{
x <- big.matrix(5, 2, init=1)
colnames(x)=c("alpha", "beta")
add.cols(x, 1, column.names="gamma")
x[,]
rm.cols(x, 2)
x[,]
add.cols(x, 2, column.names=c("jay", "mike"))
rm.cols(x, "alpha")
x[,]
}
\keyword{classes}
\keyword{methods}
