\name{big.sub.matrix}
\alias{big.sub.matrix}
\title{Create a submatrix of a big.matrix.}
\description{
This doesn't create a copy, it just provides a new version of the class
which provides behavior for a contiguous submatrix of the big.matrix.
}
\usage{
big.sub.matrix(descriptor, firstRow = 1, lastRow = NULL,
               firstCol = 1, lastCol = NULL, backingpath='' )
}
\arguments{
  \item{descriptor}{a description of the \code{\link{big.matrix}}, not the \code{big.matrix} itself; this seems odd, we realize -- trust us.}
  \item{firstRow}{the first row of the submatrix.}
  \item{lastRow}{the last row of the submatrix if not \code{NULL}.}
  \item{firstCol}{the first column of the submatrix.}
  \item{lastCol}{the last column of the submatrix if not \code{NULL}.}
  \item{backingpath}{required path to the filebacked object, if applicable.}
}
\details{
This is currently used only for our iterator.
}
\value{
A \code{\link{big.matrix}} which is actually a submatrix of a larger \code{big.matrix}.
It is not a copy, and shares the exact same mutexes as the parent matrix.  The only
practical limitations are that you can't take a submatrix of a submatrix, and you can't
do a \code{link{describe}} of a submatrix.  Only contiguous blocks may form a submatrix.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{

  cat("Need example for big.sub.matrix.\n")

}
\keyword{methods}
