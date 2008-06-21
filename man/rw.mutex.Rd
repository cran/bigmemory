\name{rw.mutex, attach.rw.mutex}
\alias{rw.mutex}
\alias{attach.rw.mutex}
\title{Mutual exclusion functionality for shared memory matrices in package ``bigmemory''}
\description{
These functions provide additional read/write mutexes (mutual exclusions) for use
with \code{big.matrix} objects in shared memory (class \code{\link{big.matrix}} of
\pkg{bigmemory}).  They are probably unnecessary for most users.
}
\usage{
rw.mutex()
attach.rw.mutex(mutexId)
}
\arguments{
  \item{mutexId}{the shared memory key for the mutex.}
}
\details{
Mutexes are provided with every shared matrix and managed transparently by the various
functions provided for \code{\link{big.matrix}} objects (\code{\link{mwhich}},
for example).  However, the user may create an additional layer of mutexes
using these functions.  This may be important in certain shared memory applications.

Note that \code{describe()} can be used to retrieve the \code{rw.mutex} information.
}
\value{
A new instance of type \code{rw.mutex} is returned.
}
\references{C libraries pthread, ipc, and shm are used for shared memory and mutexes.}
\author{John W. Emerson and Michael J. Kane}
\note{
Shared memory is not presently supported by the Windows version of 
\code{\link{bigmemory}} (but you wouldn't be reading this comment if you had
the Windows version).  We recommend reading the additional documentation in
\code{\link{shared.big.matrix}}, or contact us if you have questions.
}
\seealso{\code{\link{big.matrix}}, \code{\link{shared.big.matrix}}.}
\examples{
# None.
}
\keyword{methods}
