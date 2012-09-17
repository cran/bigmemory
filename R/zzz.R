
.onLoad <- function(libname, pkgname) {
  library.dynam("bigmemory", pkgname, libname)
  options(bigmemory.print.warning=TRUE)
  options(bigmemory.typecast.warning=TRUE)
  options(bigmemory.allow.dimnames=FALSE)
  options(bigmemory.default.type="double")
  packageStartupMessage("\nbigmemory >= 4.0 is a major revision since 3.1.2; please see package\nbiganalytics and http://www.bigmemory.org for more information.\n")
}

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
    options(bigmemory.default.type=NULL)
}
