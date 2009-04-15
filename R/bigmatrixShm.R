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
  return(new('rw.mutex', address=address))
}

attach.rw.mutex = function(mutexId)
{
  address = .Call('ConnectUserRWMutex', as.character(mutexId))
  return(new('rw.mutex', address=address))
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

shared.big.matrix = function(nrow, ncol, type='integer', init=NULL, 
  dimnames=NULL, separated=FALSE, backingfile=NULL,
  backingpath=NULL, preserve=TRUE)
{
  if (!is.null(backingfile))
  {
    return(filebacked.big.matrix(nrow=nrow, ncol=ncol, type=type, init=init, 
			dimnames=dimnames, separated=separated, backingfile=backingfile, 
			backingpath=backingpath, preserve=preserve))
  }
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
  address = .Call('CCreateSharedMatrix', as.double(nrow), 
    as.double(ncol), as.character(colnames), as.character(rownames), 
    as.integer(typeVal), as.double(init), as.logical(separated))
  if (is.null(address))
  {
  stop(paste("Error: Shared memory could not be allocated for instance",
		"of type big.matrix", sep=' '))
  }
  x=new("big.matrix", address=address)
  if (is.null(x))
  {
    stop("Error encountered when creating instance of type big.matrix")
  }
  return(x)
}

filebacked.big.matrix=function(nrow, ncol, type='integer', init=NULL,
  dimnames=NULL, separated=FALSE, backingfile=NULL, backingpath=NULL, 
	preserve=TRUE)
{
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix must have at least one row and one column')
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
  if (is.null(backingfile))
  {
    stop("You must specify a backingfile.")
  }
  if (is.null(backingpath))
  {
    backingpath = ''
  }
	else if (substr(backingpath, nchar(backingpath), nchar(backingpath)) != '/' ||
		substr(backingpath, nchar(backingpath)-1, nchar(backingpath)) != '\\' )
	{
		backingpath= paste(backingpath, '/', sep='')
	}
  address = .Call('CCreateFileBackedBigMatrix', as.character(backingfile), 
    as.character(backingpath), as.double(nrow), as.double(ncol), 
    as.character(colnames), as.character(rownames), as.integer(typeVal), 
    as.double(init), as.logical(separated), as.logical(preserve))
  if (is.null(address))
  {
    stop("Error encountered when creating instance of type big.matrix")
  }
  x=new("big.matrix", address=address)
  if (is.null(x))
  {
    stop("Error encountered when creating instance of type big.matrix")
  }
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
  if (is.shared.memory.big.matrix(x))
  {
    ret = list(sharedType='SharedMemory',
        sharedName=shared.name(x), nrow=nrow(x), ncol=ncol(x),
        rowNames=rownames(x), colNames=colnames(x), type=typeof(x), 
        separated=is.separated(x))
  }
  else
  {
    ret = list(sharedType='FileBacked',
        sharedName=shared.name(x), fileName=file.name(x),
        nrow=nrow(x), ncol=ncol(x),
        rowNames=rownames(x), colNames=colnames(x), type=typeof(x), 
        separated=is.separated(x))
  }
}

attach.big.matrix = function(obj, backingpath='')
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
  if (info$sharedType == 'SharedMemory')
  {
    address = .Call('CAttachSharedBigMatrix', info$sharedName, info$nrow, 
      info$ncol, as.character(info$rowNames), as.character(info$colNames), 
      as.integer(typeLength), info$separated)
  }
  else
  {
    address = .Call('CAttachFileBackedBigMatrix', info$sharedName, 
      info$fileName, backingpath, info$nrow, info$ncol, 
      as.character(info$rowNames), as.character(info$colNames), 
      as.integer(typeLength), info$separated)
  }
  if (!is.null(address)) 
    ans <- new('big.matrix', address=address)
  else 
    stop("Fatal error in attach: shared big.matrix does not exist.")
  return(ans)  
}

setGeneric('is.shared.memory.big.matrix', function(x) 
  standardGeneric('is.shared.memory.big.matrix'))

setMethod('is.shared.memory.big.matrix', signature(x='big.matrix'),
  function(x)
  {
    return(.Call('IsSharedMemoryBigMatrix', x@address))
  })

setGeneric('is.file.backed.big.matrix', function(x)
  standardGeneric('is.file.backed.big.matrix'))

setMethod('is.file.backed.big.matrix', signature(x='big.matrix'),
  function(x)
  {
    return(.Call('IsFileBackedBigMatrix', x@address))
  })

setGeneric('lockcols', function(x, cols=NULL, lockType='r') 
  standardGeneric('lockcols'))

# Note: this only supports column indices.  mmap needs to be called
# if we want to lock columns by name.
setMethod('lockcols', signature(x='big.matrix', lockType='character'),
  function(x, cols=NULL, lockType='r')
  {
    if(is.null(cols))
      cols = 1:ncol(x)
    else if (is.character(cols)) 
      cols <- mmap(cols, colnames(x))
		cols = abs(cols)
		cols = cols[ cols >= 1 & cols <= ncol(x)]
    if (lockType != 'r' & lockType != 'w')
      stop('Unknow lock type')
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

setGeneric('shared.name', function(x) standardGeneric('shared.name'))

setMethod('shared.name', signature(x='big.matrix'),
  function(x)
  {
    if (!is.shared(x))
      stop("The specified argument is not shared.")
    return(.Call('SharedName', x@address))
  })

setGeneric('file.name', function(x) standardGeneric('file.name'))

setMethod('file.name', signature(x='big.matrix'),
  function(x)
  {
    if (!is.file.backed.big.matrix(x))
    {
      stop("The argument is not a file backed big.matrix.")
    }
    return(.Call('FileName', x@address))
  })

setGeneric('unlockcols', function(x, cols=NULL) standardGeneric('unlockcols'))

setMethod('unlockcols', signature(x='big.matrix'),
  function(x, cols=NULL)
  {
    if (is.null(cols))
      cols = 1:ncol(x)
    else if (is.character(cols)) 
      cols <- mmap(cols, colnames(x))
		cols = abs(cols)
		cols = cols[ cols >= 1 & cols <= ncol(x)]
    .Call('BigMatrixRelease', x@address, as.double(cols))
    invisible(NULL)
  })
