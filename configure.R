cppFlags <- 'PKG_CPPFLAGS=-I../inst/include'
pkgLibs <- ''

if (Sys.info()[['sysname']] == 'Linux')
{
  pkgLibs <- 'PKG_LIBS=-lrt'
  cppFlags <- paste(cppFlags, '-DLINUX')
}

# Is it a REvo build?
if (Sys.info()[['sysname']] == "Windows" & version$os == 'intel64')
{
  cppFlags <- paste(cppFlags, '-GX')
}
if (Sys.info()[['sysname']] == "Windows" & version$system == 'x86_64, mingw32')
{
  cppFlags <- paste(cppFlags, '-DINTERLOCKED_EXCHANGE_HACK')
}

configText <- paste(cppFlags, pkgLibs, sep="\n")
write( configText, 'src/Makevars' )
