\name{write.big.matrix, read.big.matrix}
\alias{write.big.matrix}
\alias{read.big.matrix}
\title{File interface for a ``big.matrix''}
\description{
Create a \code{\link{big.matrix}} by reading from a
suitably-formatted ASCII file, or
write the contents of a \code{\link{big.matrix}} to a file.
}
\usage{
write.big.matrix(x, fileName = NA, row.names = FALSE,
                 col.names = FALSE, sep=',')
read.big.matrix(fileName, sep = ',', header = FALSE,
                row.names = NULL, col.names = NULL, type = NA,
                skip = 0, separated = FALSE, shared = FALSE,
                backingfile = NULL, backingpath = NULL,
                descriptorfile = NULL, preserve = TRUE, extraCols = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{\link{big.matrix}}.}
  \item{fileName}{the name of an input/output file.}
  \item{sep}{a field delimiter.}
  \item{header}{if \code{TRUE}, the first line (after a possible skip) should contain column names.}
  \item{row.names}{if \code{TRUE}, use the first column of the file for row names; if a vector of names, use them even if row names appear to exist in the file.}
  \item{col.names}{if \code{TRUE}, use the first row of the file for column names; if a vector of names, use them even if column names exist in the file.}
  \item{type}{preferably specified, \code{"integer"} for example.}
  \item{skip}{number of lines to skip at the head of the file.}
  \item{separated}{use separated column organization of the data instead of column-major organization.}
  \item{shared}{if \code{TRUE}, load the object into shared memory.}
  \item{backingfile}{the root name for the file(s) for the cache of \code{x}.}
  \item{backingpath}{the path to the directory containing the file backing cache.}
  \item{descriptorfile}{the file to be used for the description of the filebacked matrix.}
  \item{preserve}{if this is a filebacked \code{big.matrix}, it is preserved, by default, even after the end of the R session unless this option is set to \code{FALSE}.}
  \item{extraCols}{the optional number of extra columns to be appended to the matrix for future use.}
}
\details{
Currently, files must contain only one atomic type
(all \code{integer}, for example).
Once (if) we implement something like \code{big.data.frame}, this assumption will be relaxed.
We have other ideas for useful options as well, including the reading and writing
of subsets of columns.

When reading from a file, if \code{type} is not specified we try to
make a reasonable guess for you without
making any guarantees at this point.  The same is true for the field
separator.  Warning messages will be printed to alert you of this.
Unless you have really large integer values, we strongly recommend
you consider \code{"short"}.  If you have something that is essentially
categorical, you might even be able use \code{"char"}, with huge memory
savings in large data sets.
}
\value{
a \code{\link{big.matrix}} object is returned by \code{read.big.matrix}, while
\code{write.big.matrix} creates an output file in the present working directory.
}
\author{John W. Emerson and Michael J. Kane }
\seealso{\code{\link{big.matrix}}}
\examples{
# Without specifying the type, this big.matrix x will hold integers.
x <- as.big.matrix(matrix(1:10, 5, 2))
x[2,2] <- NA
x[,]
write.big.matrix(x, "foo.txt")

# Just for fun, I'll read it back in as character (1-byte integers):
y <- read.big.matrix("foo.txt", type="char")
y[,]

# Other examples:
w <- as.big.matrix(matrix(1:10, 5, 2), type='double')
w[1,2] <- NA
w[2,2] <- -Inf
w[3,2] <- Inf
w[4,2] <- NaN
w[,]
write.big.matrix(w, "bar.txt")
w <- read.big.matrix("bar.txt", type="double")
w[,]
w <- read.big.matrix("bar.txt", type="short")
w[,]

# Another example using row names (which we don't like).
x <- as.big.matrix(as.matrix(iris), type='double')
rownames(x) <- as.character(1:nrow(x))
write.big.matrix(x, 'IrisData.txt', col.names=TRUE, row.names=TRUE)
y <- read.big.matrix("IrisData.txt", header=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
