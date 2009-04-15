\name{big.matrix-class}
\docType{class}
\alias{big.matrix-class}
\alias{[<-,big.matrix,ANY,ANY-method}
\alias{[<-,big.matrix,ANY,missing-method}
\alias{[<-,big.matrix,missing,ANY-method}
\alias{[<-,big.matrix,missing,missing-method}
\alias{[,big.matrix,ANY,ANY,missing-method}
\alias{[,big.matrix,ANY,missing,missing-method}
\alias{[,big.matrix,missing,ANY,missing-method}
\alias{[,big.matrix,missing,missing,missing-method}
\alias{colmax,big.matrix-method}
\alias{colmean,big.matrix-method}
\alias{colmin,big.matrix-method}
\alias{colrange,big.matrix-method}
\alias{colsd,big.matrix-method}
\alias{colvar,big.matrix-method}
\alias{colsum,big.matrix-method}
\alias{colprod,big.matrix-method}
\alias{binit,big.matrix-method}
\alias{describe,big.matrix-method}
\alias{dim,big.matrix-method}
\alias{dimnames<-,big.matrix,list-method}
\alias{dimnames,big.matrix-method}
\alias{head,big.matrix-method}
\alias{max,big.matrix-method}
\alias{mean,big.matrix-method}
\alias{min,big.matrix-method}
\alias{sum,big.matrix-method}
\alias{prod,big.matrix-method}
\alias{ncol,big.matrix-method}
\alias{nrow,big.matrix-method}
\alias{print,big.matrix-method}
\alias{range,big.matrix-method}
\alias{summary,big.matrix-method}
\alias{tail,big.matrix-method}
\alias{write.big.matrix,big.matrix,character-method}
\alias{read.big.matrix,character-method}
\alias{is.shared,big.matrix-method}
\alias{is.shared,matrix-method}
\alias{is.separated,matrix-method}
\alias{is.separated,big.matrix-method}
\alias{is.filebacked,matrix-method}
\alias{is.filebacked,big.matrix-method}
\alias{typeof,big.matrix-method}
\alias{mwhich,big.matrix,ANY,ANY,ANY,character-method}
\alias{mwhich,big.matrix,ANY,ANY,ANY,missing-method}
\alias{mwhich,matrix,ANY,ANY,ANY,character-method}
\alias{mwhich,matrix,ANY,ANY,ANY,missing-method}

\title{Class ``big.matrix''.}
\description{The \code{big.matrix} class is designed for matrices with 
elements of type double, integer, short, or char.  
A \code{big.matrix} acts much like a traditional R matrix, but helps protect 
the user from many inadvertant memory-consuming pitfalls of traditional R 
matrices and data frames.  The objects may be allocated to shared memory,
and if file backing is used they may exceed virtual memory in size.  Sadly,
32-bit R constraints on Windows and many MacOS machines will be a limiting
factor with file-backed matrices.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("big.matrix", ...)}.
The functions \code{big.matrix()}, \code{shared.big.matrix()},
and \code{filebacked.big.matrix()} are intended for the user.
}
\section{Slots}{
	 \describe{
    \item{\code{address}:}{Object of class \code{"externalptr"} points to the memory location of the C++ data structure.}
  }
}
\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "numeric", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "character", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "missing", value = "numeric")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "missing", j = "numeric", value = "numeric")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "missing", j = "character", value = "numeric")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "missing", j = "missing", value = "numeric")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "numeric", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "character", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "missing", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "numeric", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "character", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "missing", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "logical", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "logical", j = "logical", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "logical", j = "missing", drop = "missing")}: ... }
    \item{colmax}{\code{signature(x = "big.matrix")}: returns the maximum of each column (or the specified columns, optionally). }
    \item{colmean}{\code{signature(x = "big.matrix")}: returns the mean of each column (or the specified columns, optionally). }
    \item{colmin}{\code{signature(x = "big.matrix")}: returns the min of each column (or the specified columns, optionally). }
    \item{colrange}{\code{signature(x = "big.matrix")}: returns the range of each column (or the specified columns, optionally). }
    \item{colsd}{\code{signature(x = "big.matrix")}: returns the standard deviation of each column (or the specified columns, optionally). }
    \item{colvar}{\code{signature(x = "big.matrix")}: returns the variance of each column (or the specified columns, optionally). }
    \item{binit}{\code{signature(x = "big.matrix")}: calculates 1D and 2D binning.}
    \item{describe}{\code{signature(x = "big.matrix")}: this is only interesting if the \code{big.matrix} is in shared memory. }
    \item{dim}{\code{signature(x = "big.matrix")}: returns the dimension of the \code{big.matrix}. }
    \item{dimnames<-}{\code{signature(x = "big.matrix", value = "list")}: set the row and column names. }
    \item{dimnames}{\code{signature(x = "big.matrix")}: get the row and column names. }
    \item{head}{\code{signature(x = "big.matrix")}: get the first 6 (or \code{n}) rows. }
    \item{max}{\code{signature(x = "big.matrix")}: returns the maximum of all the values. }
    \item{mean}{\code{signature(x = "big.matrix")}: returns the mean of all the values. }
    \item{min}{\code{signature(x = "big.matrix")}: returns the minimum of all the values.}
    \item{ncol}{\code{signature(x = "big.matrix")}: returns the number of columns.  }
    \item{nrow}{\code{signature(x = "big.matrix")}: returns the number of rows. }
    \item{print}{\code{signature(x = "big.matrix")}: a traditional \code{print()} is intentionally disabled, and returns \code{head(x)} unless \code{options()$bm.print.warning==FALSE}; in this case, \code{print(x[,])} is the result, which could be very big! }
    \item{range}{\code{signature(x = "big.matrix")}: returns the range of all the values.}
    \item{summary}{\code{signature(object = "big.matrix")}: produce a summary of each of the columns, similar (but not identical to) the traditional \code{summary()} function. }
    \item{tail}{\code{signature(x = "big.matrix")}: returns the last 6 (or \code{n}) rows. }
    \item{write.big.matrix}{\code{signature(bigMat = "big.matrix", fileName = "character")}: produce an ASCII file from the \code{big.matrix}. }
    \item{is.shared}{\code{signature(x = "big.matrix")}: return \code{TRUE} if the \code{big.matrix} is in shared memory.}
    \item{is.separated}{\code{signature(x = "big.matrix") }: return \code{TRUE} if the \code{big.matrix} is organized as a separated column vectors.}
    \item{typeof}{\code{signature(x = "big.matrix")}: return the type of the atomic elements of the \code{big.matrix}.}
	 }
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{
showClass("big.matrix")
}
\keyword{classes}
