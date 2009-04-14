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
\author{Michael J. Kane and John W. Emerson}
\section{Warning}{Mutexes are not for the faint of heart.  But if you want an additional set of locks 
beyond those already implemented with a shared matrix, you'll need this.  
We recommend you contact us if you have questions.} 
\seealso{\code{\link{big.matrix}}, \code{\link{shared.big.matrix}},
\code{\link{filebacked.big.matrix}}, \code{\link{rw.mutex}}, \code{\link{attach.rw.mutex}},
\code{\link{rlock}}, \code{\link{rwlock}}, \code{\link{unlock}}.
}
\examples{
showClass("rw.mutex")
}
\keyword{classes}
