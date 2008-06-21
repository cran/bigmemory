\name{hash.mat}
\alias{hash.mat}
\title{Create a hash into a ``big.matrix''}
\description{
Create a hash into a \code{\link{big.matrix}} based on the values of
the specified column.
}
\usage{
hash.mat(x, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{\link{big.matrix}} sorted by column \code{col}.}
  \item{col}{an integer or name of the target column; \code{x} must be sorted on this column.}
}
\details{
When a column of a \code{\link{big.matrix}} contains many duplicated values,
it can be useful to access subsets of the matrix using
a hash table.  To do this, the matrix must first be sorted on the
entries in the desired column, and the code is designed for integer (or char or short)
\code{\link{big.matrix}} matrices.  Ideally, the values in the specified column
should range from 1 to some maximum value that is considerably less than
\code{nrow(x)}.
}
\value{
a two-column matrix, where the values in row \code{i} provide the range of
indices of \code{x} containing the value \code{i} in the specified column,
\code{col}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{
x <- as.big.matrix(matrix(sample(1:10, 200, replace=TRUE), 50, 4))
theorder <- order(x[,1])
for (i in 1:ncol(x)) x[,i] <- x[theorder,i]
thehash <- hash.mat(x, 1)
x[,]
thehash

# The following will produce all rows with entries 5 or 9 in the first column:
x[c(thehash[5,1]:thehash[5,2], thehash[9,1]:thehash[9,2]),]
}
\keyword{classes}
\keyword{methods}
