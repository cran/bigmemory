# /*
#  *  bigmemory: an R package for managing massive matrices using C,
#  *  with support for shared memory.
#  *
#  *  Copyright (C) 2008 John W. Emerson and Michael J. Kane
#  *
#  *  This file is part of bigmemory.
#  *
#  *  bigmemory is free software; you can redistribute it and/or modify
#  *  it under the terms of the GNU Lesser General Public License as published
#  *  by the Free Software Foundation; either version 3 of the License, or
#  *  (at your option) any later version.
#  *
#  *  This program is distributed in the hope that it will be useful,
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  *  GNU Lesser General Public License for more details.
#  *
#  *  You should have received a copy of the GNU Lesser General Public License
#  *  along with this program; if not, a copy is available at
#  *  http://www.r-project.org/Licenses/
#  */


setClass('rw.mutex', representation(address='externalptr'))

rw.mutex = function()
{
  address = .Call('CreateUserRWMutex')
  return(new('UserRWMutex', address=address))
}

attach.rw.mutex = function(mutexId)
{
  address = .Call('ConnectUserRWMutex', as.integer(mutexId))
  return(new('UserRWMutex', address=address))
}

setGeneric('describe', function(x) 
  standardGeneric('describe'))

setMethod('describe', signature(x='rw.mutex'),
  function(x)
  {
    return(.Call('GetUserRWMutexInfo', x@address))
  })

# If more types of mutexes are supported, then these functions should
# be made generic.
rlock = function(x)
{
  .Call('RLockUserRWMutex', x@address)
  invisible(NULL)
}

rwlock = function(x)
{
  .Call('RWLockUserRWMutex', x@address)
  invisible(NULL)
}

unlock = function(x)
{
  .Call('UnlockUserRWMutex', x@address)
  invisible(NULL)
}

shared.big.matrix = function(nrow, ncol, type='integer', init=0, 
  dimnames=NULL) #colnames=NULL, rownames=NULL)
{
  # It would be nice if init could be a vector or matrix.
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix must have at least one row and one column')

  typeVal=NULL
  if (type == 'integer') 
    typeVal=4
  if (type == 'double') 
    typeVal=8
  if (type == 'short') 
    typeVal=2
  if (type == 'char') 
    typeVal=1
  if (is.null(typeVal)) stop('invalid type')
  if (!is.null(dimnames)) {
    rownames <- dimnames[[1]]
    colnames <- dimnames[[2]]
  } else {
    rownames <- NULL
    colnames <- NULL
  }
  address = .Call('CCreateSharedMatrix', nrow=as.double(nrow), 
    ncol=as.double(ncol), colnames=as.character(colnames), 
    rownames=as.character(rownames), 
    type=as.integer(typeVal), init=as.double(init))
  x=new("big.matrix", address=address)
  if (is.null(x))
    stop("Error encountered when creating instance of type big.matrix")
  return(x)
}

setMethod('describe', signature(x='big.matrix'),
  function(x)
  {
    return(DescribeBigSharedMatrix(x))
  })

DescribeBigSharedMatrix = function(x) #, file=NULL, path="")
{
  if (!is.shared(x)) stop("this is not a shared big.matrix")

  ret = append( list(type=typeof(x), nrow=nrow(x), rowNames=rownames(x), 
      ncol=ncol(x), colNames=colnames(x)), 
      .Call('GetBigSharedMatrixInfo',x@address) )
}

attach.big.matrix = function(obj)
{
  if (is.list(obj)) 
    info <- obj
  else 
    stop("Error in AttachBigSharedMatrix: argument is not a list.\n")

  typeLength=NULL
  if (info$type == 'integer') 
    typeLength=4
  if (info$type == 'double') 
    typeLength=8
  if (info$type == 'short') 
    typeLength=2
  if (info$type == 'char') 
    typeLength=1
  if (is.null(typeLength)) stop('invalid type')

  address = .Call('CAttachSharedMatrix', info$ncol, info$colNames, 
    info$nrow, info$rowNames, as.integer(typeLength), 
    info$colKeys, info$colMutexKeys, info$shCountKey, info$shCountMutexKey)
  if (!is.null(address)) 
    ans <- new('big.matrix', address=address)
  else 
    stop("Fatal error in attach: shared big.matrix does not exist.")
  return(ans)  
}

# Currently, this is not being exported.  It needs to be tested.
pack.shared.objects=function(objects, file="")
{
  if (!(length(objects)==1 || class(objects)=='list'))
  {
    stop("You must provide an element or list of elements to pack.")
  }
  c1 = match.call()
  packVars = deparse(c1$objects)
  packVars = unlist(strsplit(as.character(packVars), "\\("))[2]
  packVars = unlist(strsplit(as.character(packVars), "\\)"))[1]
  packVars = unlist(strsplit(as.character(packVars), ","))
  packVars = gsub('\\s+', '', packVars, perl=TRUE)
  packInfo=list()
  for (i in 1:length(objects))
  {
    if (class(objects[[i]]) == 'big.matrix')
    {
      if (!is.shared(objects[[i]]))
        stop("You may not pack a big.matrix that is not shared")
      packInfo = append(packInfo, list(append(list(class=class(objects[[i]]), 
        varName=packVars[i]), describe(objects[[i]]))))
    }
    if (class(objects[[i]]) == 'rw.mutex')
    {
      packInfo = append(packInfo, list(append(list(class=class(objects[[i]]), 
        varName=packVars[i]), list(keys=describe(objects[[i]])))))
    }
  }
  dput(packInfo, file)
  invisible(NULL)
}

# Currently, this is not being exported.  It needs to be tested.
unpack.shared.objects=function(file, global=TRUE)
{
  shInfo = dget(file)
  ret = list()
  for (i in 1:length(shInfo))
  {
    info = shInfo[[i]]
    if (info$class == 'UserRWMutex')
    {
      if (global==TRUE) {
        assign(info$varName, attach.rw.mutex(info$keys[1]), env=.GlobalEnv)
      } 
      else 
      {
        s = paste('ret = append(ret,list(', info$varName,
          '=ConnectUserRWMutex(info$keys[1])))')
        eval(parse(text=s))
      }
    }
    if (info$class == 'big.matrix')
    {
        AttachSharedMatrix <- NA
        if (info$type == 'integer') 
          AttachSharedMatrix <- "CAttachIntSharedMatrix"
        if (info$type == 'double') 
          AttachSharedMatrix <- "CAttachDoubleSharedMatrix"
        if (info$type == 'short') 
          AttachSharedMatrix <- "CAttachShortSharedMatrix"
        if (info$type == 'char') 
          AttachSharedMatrix <- "CAttachCharSharedMatrix"
        if (is.na(AttachSharedMatrix)) 
          stop('invalid type')

        address = .Call(AttachSharedMatrix, info$ncol, info$colNames, 
          info$nrow, info$rowNames, info$colKeys, info$colMutexKeys, 
          info$shCountKey, info$shCountMutexKey)

      if (global==TRUE) 
      {
        assign(info$varName, new('big.matrix', address=address, type=info$type),
          env=.GlobalEnv)
      } 
      else 
      {
        s = paste('ret = append(ret, list(', info$varName,
          '=new("big.matrix", address=address, type=info$type)))')
        eval(parse(text=s))
      }
    }
  }
  if (global==FALSE)
    return(ret)
  invisible(NULL)
}

setGeneric('lockcols', function(x, cols, lockType='r') 
  standardGeneric('lockcols'))

# Note: this only supports column indices.  mmap needs to be called
# if we want to lock columns by name.
setMethod('lockcols', signature(x='big.matrix', lockType='character'),
  function(x, cols, lockType='r')
  {
    if (lockType != 'r' & lockType != 'w')
      stop('Unknow lock type')
    func=''
    if (lockType == 'r')
    {
      .Call('BigMatrixRLock', x@address, as.double(cols))
      invisible(NULL)
    }
    else
    {
      .Call('BigMatrixRWLock', x@address, as.double(cols))
      invisible(NULL)
    }
  })

setGeneric('unlockcols', function(x, cols) standardGeneric('unlockcols'))

setMethod('unlockcols', signature(x='big.matrix'),
  function(x, cols)
  {
    .Call('BigMatrixRelease', x@address, as.double(cols))
    invisible(NULL)
  })

shared.deepcopy <- function(x) 
{
  if (exists("shared.big.matrix", mode="function")) 
  {
    y <- shared.big.matrix(nrow=nrow(x), ncol=ncol(x),
                    type=typeof(x), dimnames=list(rownames(x), colnames(x)))
    for (i in 1:ncol(x)) y[,i] <- x[,i]
  } 
  else 
  {
    stop("This version does not support shared matrices.\n")
  }
  return(y)
}

