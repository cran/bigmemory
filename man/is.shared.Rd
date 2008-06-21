\name{is.shared}
\alias{is.shared}
\title{Check the shared memory status of a ``big.matrix''}
\description{
Check to see if a \code{big.matrix} is in shared memory.
}
\usage{
is.shared(x)
}
\arguments{
  \item{x}{a \code{\link{big.matrix}}.}
}
\value{
\code{TRUE} if a \code{\link{big.matrix}} is in shared memory, and \code{FALSE}
otherwise.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, \code{\link{big.matrix}}}
\examples{
x <- big.matrix(10, 2, init=-5)
colnames(x)=c("alpha", "beta")
is.big.matrix(x)
is.shared(x)

\dontrun{
z <- shared.big.matrix(7, 3, type='integer', init=3)
is.big.matrix(z)
is.shared(z)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{methods}
