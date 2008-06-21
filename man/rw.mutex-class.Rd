\name{rw.mutex-class}
\docType{class}
\alias{rw.mutex-class}
\alias{rw.mutex-method}
\alias{describe,rw.mutex-method}

\title{Mutual exclusions (mutexes) for shared memory.}
\description{Support for read/write mutual exclusions for objects in shared memory (separately from the mutexes used with shared matrices).}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rw.mutex", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{address}:}{Object of class \code{"externalptr"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{describe}{\code{signature(x = "rw.mutex")}: ... }
	 }
}
\references{*nix system libraries pthread, ipc, and shm.}
\author{Michael J. Kane and John W. Emerson}
\section{Warning}{Mutexes are not for the faint of heart.  But if you want an additional set of locks 
beyond those already implemented with a shared matrix, you'll need this.  
We recommend you look at the documentation in \code{\link{shared.big.matrix}}, or contact us if you
have questions.} 
\seealso{\code{\link{big.matrix}}, \code{\link{shared.big.matrix}}.
}
\examples{
showClass("rw.mutex")
}
\keyword{classes}
