\name{rw.mutex, attach.rw.mutex}
\alias{rw.mutex}
\alias{attach.rw.mutex}
\alias{rlock}
\alias{rwlock}
\alias{unlock}
\title{Mutual exclusion functionality for shared memory matrices in package ``bigmemory''}
\description{
These functions provide additional read/write mutexes (mutual exclusions) for use
with \code{big.matrix} objects in shared memory (class \code{\link{big.matrix}} of
\pkg{bigmemory}).  They are probably unnecessary for most users.
}
\usage{
rw.mutex()
attach.rw.mutex(mutexId)
rlock(x)
rwlock(x)
unlock(x)
}
\arguments{
  \item{mutexId}{the shared memory key for the mutex.}
  \item{x}{an object of type \code{rw.mutex}.}
}
\details{
Mutexes are provided with every shared matrix (including file-backed matrices) and are
managed transparently by the various
functions provided for \code{\link{big.matrix}} objects (\code{\link{mwhich}},
for example).  However, the user may create an additional layer of mutexes
using these functions.  This may be important in certain shared memory applications.

Note that \code{\link{describe}} should be used to retrieve the \code{rw.mutex} information.

Mutexes will be stripped out of bigmemory in versions 4.0 and beyond, leaving them
available (optionally) to the user in a new package, synchronicity, in the Fall of
2009.  This is largely for performance reasons with bigmemory, where the vast
majority of shared memory work is read-only or involves algorithms that don't
risk read/write conflicts.
}
\value{
A new instance of type \code{rw.mutex} is returned.
}
\references{http://www.boost.org}
\author{John W. Emerson and Michael J. Kane}
\note{
We recommend you contact us if you have questions about user mutexes, which are not for the
faint of heart.
}
\seealso{\code{\link{big.matrix}}, \code{\link{shared.big.matrix}},
\code{\link{filebacked.big.matrix}}, \code{\link{describe}}.
}
\examples{
# None.
}
\keyword{methods}
