
.onLoad <- function(libname, pkgname) {

  isNamespaceLoaded <- function(name)
  { !is.null(.Internal(getRegisteredNamespace(as.name(name)))) }
  
  if (!isNamespaceLoaded('synchronicity')) {
      setGeneric('describe', function(x) standardGeneric('describe'))
  }

  setMethod('describe', signature(x='big.matrix'),
    function(x)
    {
      return(new('big.matrix.descriptor', description=DescribeBigMatrix(x)))
    })


  library.dynam("bigmemory", pkgname, libname)
  options(bigmemory.print.warning=TRUE)
  options(bigmemory.typecast.warning=TRUE)
  options(bigmemory.allow.dimnames=FALSE)
  options(bigmemory.default.type="double")
  packageStartupMessage("\nbigmemory >= 4.0 is a major revision since 3.1.2; please see package\nbiganalytics and http://www.bigmemory.org for more information.\n")
}

#.noGenerics <- TRUE           # This was a problem, not used.

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
    options(bigmemory.default.type=NULL)
}
