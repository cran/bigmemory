\name{deepcopy}
\alias{deepcopy}
\title{Produces a physical copy of a ``big.matrix''}
\description{
This is needed to make a duplicate of a \code{big.matrix}.
}
\usage{
deepcopy(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{\link{big.matrix}}.}
}
\details{
This is needed to make a duplicate of a \code{\link{big.matrix}}, because traditional
R syntax would only copy the R object (the pointer to the \code{\link{big.matrix}}
rather than the \code{\link{big.matrix}} itself).
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
