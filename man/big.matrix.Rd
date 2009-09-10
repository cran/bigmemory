\name{big.matrix, shared.big.matrix, filebacked.big.matrix, is.big.matrix, as.big.matrix, is.shared, is.separated, is.filebacked, remove.backing, nebytes}
\alias{big.matrix}
\alias{shared.big.matrix}
\alias{filebacked.big.matrix}
\alias{as.big.matrix}
\alias{is.big.matrix}
\alias{is.separated}
\alias{is.shared}
\alias{is.filebacked}
\alias{[.big.matrix}
\alias{nebytes}
\alias{backingpath}
\title{The core ``big.matrix'' operations.}
\description{
Create a \code{big.matrix} (or check to see if an object is a \code{big.matrix},
or create a \code{big.matrix} from a \code{\link{matrix}}, and so on).  The \code{big.matrix}
may be in-memory (shared or not), or file backed (which is always shareable).
}
\usage{
big.matrix(nrow, ncol, type = "integer", init = NULL, dimnames = NULL,
           separated = FALSE, shared = FALSE, backingfile = NULL,
           backingpath = NULL, descriptorfile = NULL, preserve = TRUE,
           nebytes = 0)
shared.big.matrix(nrow, ncol, type = "integer", init = NULL,
           dimnames = NULL, separated = FALSE, backingfile = NULL,
           backingpath = NULL, descriptorfile = NULL, preserve = TRUE,
           nebytes = 0)
filebacked.big.matrix(nrow, ncol, type = "integer", init = NULL,
           dimnames = NULL, separated = FALSE, backingfile = NULL,
           backingpath = NULL, descriptorfile = NULL, preserve = TRUE,
           nebytes = 0)
as.big.matrix(x, type = NULL, separated = FALSE, shared = FALSE,
              backingfile = NULL, backingpath = NULL,
              descriptorfile = NULL, preserve = TRUE)
is.big.matrix(x)
is.separated(x)
is.shared(x)
is.filebacked(x)
nebytes(x)
backingpath(x)
}
\arguments{
  \item{x}{a \code{matrix} or \code{vector}; if a vector, a one-column \code{big.matrix} is created by \code{as.big.matrix}.}
  \item{nrow}{number of rows.}
  \item{ncol}{number of columns.}
  \item{type}{the type of the atomic element (\code{"integer"} by default).}
  \item{init}{a scalar value for initializing the matrix (NULL by default to avoid unnecessary time spent doing the initializing).}
  \item{dimnames}{a list of the row and column names.}
  \item{separated}{use separated column organization of the data; see details.}
  \item{shared}{\code{TRUE} if the \code{big.matrix} should be allocated to shared memory.}
  \item{backingfile}{the root name for the file(s) for the cache of \code{x}.}
  \item{backingpath}{the path to the directory containing the file backing cache.}
  \item{descriptorfile}{the name of the file to hold the filebacked description, for subsequent use with \code{\link{attach.big.matrix}}; if NULL, the \code{backingfile} is used as the root.  The descriptor file is placed in the same directory as the backing files.}
  \item{preserve}{if this is a filebacked \code{big.matrix}, it is preserved, by default, even after the end of the R session unless this option is set to \code{FALSE}.}
  \item{nebytes}{undocumented option to support internal experimentation}
}
\details{
A \code{big.matrix} consists of an object in R that does nothing more than point to
the data structure implemented in C++.  The object acts
much like a traditional R matrix, but helps protect the user from many inadvertant
memory-consuming pitfalls of traditional R matrices and data frames.

There are three \code{big.matrix} types which manage
data in different ways.  The basic (or default)
\code{big.matrix} is not shared across processes
and is limited to available RAM.  A shared \code{big.matrix} has identical
size constraints as the basic \code{big.matrix}, but may be shared across
separate R processes.  A file-backed \code{big.matrix} may
exceed available RAM by using hard drive space, and may also be
shared across processes.  The atomic types of these matrices may be
double, integer, short, or char (8, 4, 2, and 1 bytes, respectively).

If \code{x} is a \code{big.matrix}, then \code{x[1:5,]} is returned as an R
\code{matrix} containing the first five rows of \code{x}.  If \code{x} is of type
\code{double}, then the result will be \code{numeric}; otherwise, the result will
be an \code{integer} R matrix.  The expression \code{x} alone
will display information about the R object (e.g. the external pointer) rather
than evaluating the matrix itself (the user should try \code{x[,]} with extreme caution,
recognizing that a huge R \code{matrix} will be created).

If \code{x} has a huge number of rows, then the use of \code{rownames}
will be extremely memory-intensive and should be avoided.  If \code{x} has a huge
number of columns, the user might want to store the transpose as there is 
overhead from the mutexes for each column in the matrix.  This concern will
change in versions 4.0 and beyond when mutex will be provided optionally through
a new package, synchronicity.

If \code{separated} is \code{TRUE}, then the memory is allocated into separate
vectors for each column.  If \code{separated} is \code{FALSE}, the matrix is
stored in traditional column-major format.  
The function \code{is.separated()} returns
the separation type of the \code{big.matrix}.

When a \code{big.matrix}, \code{x}, is passed as an argument
to a function, it is essentially providing call-by-reference rather than
call-by-value behavior.  If the function modified any of the values of \code{x}
within the function, the changes are not limited in scope to
a local copy within the function.  

A shared \code{\link{big.matrix}} object is essentially the same as a non-shared 
\code{big.matrix} object except the memory being managed may be shared 
across R sessions.

A file-backed \code{big.matrix} may exceed available RAM in size by using a file
cache (or possibly multiple file caches, if \code{separated} is \code{TRUE}).
This can incur a performance penalty for large matrices, but is obviously
useful nonetheless.  A side-effect of creating a filebacked object is
not only the filebacking(s), but a descriptor file (in the same directory) that can
be used for subsequent attachments (see \code{\link{attach.big.matrix}}).
}
\value{
A \code{big.matrix} is returned (for \code{big.matrix}, \code{shared.big.matrix},
\code{filebacked.big.matrix}, and \code{as.big.matrix}),
and \code{TRUE} or \code{FALSE} for \code{is.big.matrix} and the other functions.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, and perhaps the class documentation of
\code{\linkS4class{big.matrix}}; \code{\link{attach.big.matrix}} and
\code{\link{describe}}.
}
\examples{

x <- big.matrix(10, 2, type='integer', init=-5)
colnames(x) = c("alpha", "beta")
is.big.matrix(x)
dim(x)
colnames(x)
rownames(x)
x[,]
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
gc()

x <- as.big.matrix(matrix(-5, 10, 2))
colnames(x) <- c("alpha", "beta")
is.big.matrix(x)
dim(x)
colnames(x)
rownames(x)
x[1:8,1] <- 11:18
x[,]
gc()

# The following shared memory example is quite silly, as you wouldn't likely do
# this in a single R session.  But if zdescription were passed to another R session
# via SNOW, NetWorkSpaces, or even by a simple file read/write,
# then the attach.big.matrix() within the second R process would give access to the
# same object in memory.  Please see the package vignette for real examples.
z <- shared.big.matrix(3, 3, type='integer', init=3)
z[,]
dim(z)
z[1,1] <- 2
z[,]
zdescription <- describe(z)
zdescription
y <- attach.big.matrix(zdescription)
y[,]
y
z
y[1,1] <- -100
y[,]
z[,]
gc()

# A short filebacked example, showing the creation of associated files and mutexes:
files <- dir()
files[grep("example.bin", files)]
z <- filebacked.big.matrix(3, 3, type='integer', init=123, backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list( c('a','b','c'), c('d', 'e', 'f')))
z[,]
files <- dir()
files[grep("example.bin", files)]
zz <- attach.big.matrix("example.desc")
zz[,]
zz[1,1] <- 0
zzz <- attach.big.matrix(describe(z))
zzz[,]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{methods}
