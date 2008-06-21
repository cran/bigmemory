\name{big.matrix, is.big.matrix, as.big.matrix}
\alias{big.matrix}
\alias{as.big.matrix}
\alias{is.big.matrix}
\title{The basic ``big.matrix'' operations.}
\description{
Create a \code{big.matrix} (or check to see if an object is a \code{big.matrix},
or create a \code{big.matrix} from a \code{\link{matrix}}).
}
\usage{
big.matrix(nrow, ncol, type = "integer", init = 0, dimnames = NULL)
as.big.matrix(x, type = "integer")
is.big.matrix(x)
}
\arguments{
  \item{x}{an object; if a vector, a one-column \code{big.matrix} is created by \code{as.big.matrix}.}
  \item{nrow}{number of rows.}
  \item{ncol}{number of columns.}
  \item{type}{the type of the atomic element (\code{"integer"} by default).}
  \item{init}{a scalar value for initializing the matrix (0 by default).}
  \item{dimnames}{a list of the row and column names.}
}
\details{
A \code{big.matrix} consists of an object in R that does little more than point to
the data structure implemented in C++.  The object acts
much like a traditional R matrix, but helps protect the user from many inadvertant
memory-consuming pitfalls of traditional R matrices and data frames.

Four atomic types are implemented (see argument \code{type}, above) to
help provide memory efficiency in different applications: \code{double}
(equivalent to \code{numeric} in R), \code{integer} (using 4 bytes), \code{short}
(using 2 bytes), and \code{char} (using a single byte).  

If \code{x} is a \code{big.matrix}, then \code{x[1:5,]} is returned as an R
\code{matrix} containing the first five rows of \code{x}.  If \code{x} is of type
\code{double}, then the result will be \code{numeric}; otherwise, the result will
be an \code{integer} R matrix.  The expression \code{x} alone
will display information about the R object (e.g. the type) rather than evaluating the
matrix itself (the user should try \code{x[,]} with extreme caution,
recognizing that a huge R \code{matrix} will be created).

If \code{x} has a huge number of rows, then the use of \code{rownames}
will be extremely memory-intensive and should be avoided.  If \code{x} has a huge
number of columns, the user might want to store the transpose as there is 
overhead of a pointer for each column in the matrix.

Finally, when a \code{big.matrix}, \code{x}, is passed as an argument
to a function, it is essentially providing call-by-reference rather than
call-by-value behavior.  If the function modified any of the values of \code{x}
within the function, the changes are not limited in scope to
a local copy within the function.  
}
\value{
A \code{big.matrix} is returned (for \code{big.matrix} and \code{as.big.matrix}),
and \code{TRUE} or \code{FALSE} for \code{is.big.matrix}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, and perhaps the class documentation of
\code{\linkS4class{big.matrix}}.}
\examples{

x <- big.matrix(10, 2, type='integer', init=-5)
colnames(x) = c("alpha", "beta")
is.big.matrix(x)
dim(x)
colnames(x)
rownames(x)
x[1:8,1] <- 11:18
x[,]
colmin(x)
colmax(x)
colrange(x)
colsum(x)
colprod(x)
colmean(x)
colvar(x)
summary(x)

x <- as.big.matrix(matrix(-5, 10, 2))
colnames(x) <- c("alpha", "beta")
is.big.matrix(x)
dim(x)
colnames(x)
rownames(x)
x[1:8,1] <- 11:18
x[,]
colmin(x)
colmax(x)
colsum(x)
colprod(x)
colmean(x)
colvar(x)
colrange(x)
summary(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{methods}
