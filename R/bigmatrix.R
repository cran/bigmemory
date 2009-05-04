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


setClass("big.matrix", representation(address='externalptr'))

big.matrix = function(nrow, ncol, type='integer', init=NULL, dimnames=NULL,
                      separated=FALSE, shared=FALSE, backingfile=NULL, 
											backingpath=NULL, descriptorfile=NULL, preserve=TRUE)
{
  # It would be nice if init could be a vector or matrix.
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix instance must have at least one row and one column')
  
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
  address=NULL
  if (is.null(backingfile) & !shared)
  {
    address = .Call('CCreateMatrix', as.double(nrow), as.double(ncol),
      as.double(init), as.integer(typeVal), as.logical(separated))
    if (is.null(address))
    {
      stop(paste("Error: Memory could not be allocated for instance of",
				"type big.matrix", sep=' '))
    }
    x = new("big.matrix", address=address)
    if (is.null(x))
      stop("Error encountered when creating instance of type big.matrix")
    dimnames(x) = dimnames
    return(x)
  }
  else
  {
		if (!is.null(backingpath) &&
			substr(backingpath, nchar(backingpath), nchar(backingpath)) != '/' ||
			substr(backingpath, nchar(backingpath)-1, nchar(backingpath)) != '\\' )
		{
			backingfile = paste(backingpath, '/', sep='')
		}
    return(shared.big.matrix(nrow=nrow, ncol=ncol, type=type, init=init, 
			dimnames=dimnames, separated=separated, backingfile=backingfile, 
			backingpath=backingpath, descriptorfile=descriptorfile, 
      preserve=preserve))
  }
}

is.big.matrix <- function(x) return(class(x) == "big.matrix")

as.big.matrix <- function(x, type = NULL, separated = FALSE,
  shared=FALSE, backingfile=NULL, backingpath=NULL, descriptorfile=NULL,
  preserve=TRUE)
{
  if (is.vector(x)) {
    x <- matrix(x, length(x), 1)
    warning("Coercing vector to a single-column matrix.")
  }
  if (!is.matrix(x)) 
		stop('argument is not a matrix; perhaps it is a data frame?')
	if (!is.numeric(x))
	{
		warning("Casting matrix to numeric type")
		x = matrix( as.numeric(x), nrow=nrow(x), dimnames=dimnames(x) )
	}
	if (is.null(type))
	{
		type = typeof(x)
	}
  
  if (type=="integer" | type=="double" | type=="short" | type=="char") 
  {
    if (shared | !is.null(backingfile))
    {
      y = shared.big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, init=NULL, 
				dimnames=dimnames(x), separated=separated, backingfile=backingfile, 
				backingpath=backingpath, descriptorfile=descriptorfile,
        preserve=preserve)
    }
		else
    {
      y <- big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, init=NULL, 
				dimnames=dimnames(x), separated=separated)
    }
    y[1:nrow(x),1:ncol(x)] <- x
    junk <- gc() 
  } else stop('that type is not implemented.')
  return(y)
}

colnames.bm = function(x)
{
  ret = .Call("GetColumnNamesBM", x@address)
  if (length(ret)==0) return(NULL)
  return(ret)
}

rownames.bm = function(x)
{
  ret = .Call("GetRowNamesBM", x@address)
  if (length(ret)==0) return(NULL)
  return(ret)
}

assign('colnames.bm<-', 
  function(x,value) {
    if (!is.shared(x)) {
      if (is.character(value)) {
        if (any(value=="")) {
          value <- NULL
          warning("empty strings prohibited in column names")
        }
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          warning("column names coerced to character")
        }
      }
      if (!is.null(value) & length(value) != ncol(x))
      {
        stop("length of 'colnames' not equal to array extent.")
      }
      .Call("SetColumnNames", x@address, value)
      return(x)
    } else stop('changing column names of a shared object is prohibited.')
})

assign('rownames.bm<-',
  function(x,value) {
    if (!is.shared(x)) {
      if (is.character(value)) {
        if (any(value=="")) {
          value = NULL
          warning("empty strings prohibited in row names")
        }
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          warning("row names coerced to character")
        }
      }
      if (length(value) != nrow(x) & !is.null(value)) 
        stop("length of 'rownames' not equal to array extent.")
      .Call("SetRowNames", x@address, value)
      return(x)
    } else stop('changing row names of a shared object is prohibited.')
})

setMethod('ncol', signature(x="big.matrix"),
  function(x) {
    return(.Call("CGetNcol", x@address))
})

setMethod('nrow', signature(x="big.matrix"), 
  function(x) {
    return(.Call("CGetNrow", x@address))
})

setMethod('dim', signature(x="big.matrix"),
  function(x) return(c(nrow(x), ncol(x))))

'[.big.matrix' = function(x, i=NULL, j=NULL)
{
  if (is.shared(x) && options()$rlock.enabled)
	{
    lockcols(x, j, 'r')
	}
  retList <- .Call("GetMatrixElements", x@address, j, i)

  dimnames(retList[[1]]) = list( retList[[2]], retList[[3]] )
  if (sum(dim(retList[[1]])==1) > 0)
  {
    retList[[1]] = as.vector(retList[[1]])
  }
  if (is.shared(x) && options()$rlock.enabled)
	{
    unlockcols(x,j)
	}
  return(retList[[1]])
}

SetFunction.bm = function(x, i, j, value)
{
  if (is.shared(x))
  {
    options(rlock.enabled=FALSE)
    lockcols(x, j, 'w')
  }
  # Note: i may be a mwhich statement in which case we _must_ ensure
  # that we disable read locking before it is evaluated or we will
  # have a race condition.
  # Make sure we are setting at valid indices.
  if (max(j) > ncol(x) | min(j) < 1 | max(i) > nrow(x) | min(i) < 1)
  {
    if (is.shared(x))
    {
      unlockcols(x, j)
      options(rlock.enabled=TRUE)
    }
    stop('Indices out of range')
  }

  totelts <- as.numeric(length(i)) * as.numeric(length(j))
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value)){
    if (ncol(value) != length(j) | nrow(value) != length(i))
    {
      cat("ERROR check", ncol(x), length(j), nrow(x), length(i), "\n")
      if (is.shared(x))
      {
        options(rlock.enabled=TRUE)
        unlockcols(x, j)
      }
      stop("Matrix dimensions do not agree with big.matrix instance set size")
    }
  } else if (length(value) != totelts) {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps = totelts / length(value)
    if (numReps != round(numReps)) {
      stop("number of items to replace is not a multiple of replacement length")
    }
    if (totelts > .Machine$integer.max) stop("Too large an assignment for R; bigmemory can fix this, eventually.\n")
    value = rep(value, numReps)
  }
  if (typeof(x) != 'double')
  {
    integerVals = na.omit(as.integer(value))
    if (sum(integerVals == na.omit(as.integer(value))) != length(integerVals) |
        is.factor(value)) {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
  if (typeof(x) == 'double') {
    .Call("SetMatrixElements", x@address, as.double(j), as.double(i), 
			as.double(value))
  } else {
    .Call("SetMatrixElements", x@address, as.double(j), as.double(i), 
			as.integer(value))
  }
  if (is.shared(x))
  {
    unlockcols(x,j)
    options(rlock.enabled=TRUE)
  }
  return(x)
}

setMethod('[<-',
  signature(x = "big.matrix"),
  function(x, i, j, value) {
    if (!is.numeric(i) & !is.character(i) & !is.logical(i))
      stop("row indices must be numeric, logical, or character vectors.")
    if (!is.numeric(j) & !is.character(j) & !is.logical(j))
      stop("column indices must be numeric, logical, or character vectors.")
    if (is.character(i))
      if (is.null(rownames(x))) stop("row names do not exist.")
      else i <- mmap(i, rownames(x))
    if (is.character(j))
      if (is.null(colnames(x))) stop("column names do not exist.")
      else j <- mmap(j, colnames(x))
    if (is.logical(i)) {
      if (length(i) != nrow(x))
        stop("row vector length must match the number of rows of the matrix.")
      i <- which(i)
    }
    if (is.logical(j)) {
      if (length(j) != ncol(x))
        stop(paste("column vector length must match the number of",
                   "columns of the matrix."))
      j <- which(j)
    }
    if ( options()$bigmemory.typecast.warning &&
         (typeof(value) == "double") && (typeof(x) != "double") ||
         (typeof(value) == "integer" && 
          (typeof(x) != "double" && typeof(x) != "integer")) )
    {
      warning(cat("Assignment will down cast from ", typeof(value), " to ",
        typeof(x), "\nHint: To remove this warning type:  ",
				"options(big.memory.typecast.warning=FALSE)\n", sep=''))
    }
    return(SetFunction.bm(x, i, j, value))
  })

setMethod('[<-',
  signature(x = "big.matrix", i="missing"),
  function(x, j, value) return('[<-'(x, 1:nrow(x), j, value=value)))

setMethod('[<-',
  signature(x = "big.matrix", j="missing"),
  function(x, i, value) return('[<-'(x, i, 1:ncol(x), value=value)))

setMethod('[<-',
  signature(x = "big.matrix", i="missing", j="missing"),
  function(x, value) return(SetFunction.bm(x, 1:nrow(x), 1:ncol(x), value=value)))

setMethod('typeof', signature(x="big.matrix"),
  function(x) return(.Call('GetTypeString', x@address)))

setMethod('head', signature(x="big.matrix"),
  function(x, n = 6) {
    n <- as.integer(n)
    if (n<1 | n>nrow(x)) stop("n must be between 1 and nrow(x)")
    return(x[1:n,])
  })

setMethod('tail', signature(x="big.matrix"),
  function(x, n = 6) {
    n <- as.integer(n)
    if (n<1 | n>nrow(x)) stop("n must be between 1 and nrow(x)")
    return(x[(nrow(x)-n+1):nrow(x),])
  })

setMethod('print', signature(x='big.matrix'), 
  function(x) {
    if (options()$bigmemory.print.warning==TRUE)
    {
      cat("Warning: This is not advised.  Here is the head of the matrix:\n")
      print(head(x))
    }
    else
    {
      # Should change this to a C print function, unfortunately, for proper
      # formatting, this means we would also have to pass the terminal
      # width.
      print(x[,])
    }
  })

###################################################################
# x big.matrix  
# cols  is.numeric or is.character
# vals  list of scalar or 2-vectors otherwise
# comps could be missing, in which case we'll fill in 'eq' in signature, earlier
#   list of comparisons matching dim of associated vals component

setGeneric('mwhich', function(x, cols, vals, comps, op = 'AND')
  standardGeneric('mwhich'))

setMethod('mwhich',
  signature(x='big.matrix', op='character'),
  function(x, cols, vals, comps, op)
  {
    return(mwhich.internal(x, cols, vals, comps, op, 'MWhichBigMatrix'))
  })

setMethod('mwhich',
  signature(x='matrix', op='character'),
  function(x, cols, vals, comps, op)
  {
    if (is.integer(x))
      return(mwhich.internal(x, cols, vals, comps, op, 'MWhichRIntMatrix'))
    if (is.numeric(x))
      return(mwhich.internal(x, cols, vals, comps, op, 'MWhichRNumericMatrix'))
    stop("Unsupported matrix type given to mwhich")
  })

setMethod('mwhich',
  signature(x='big.matrix', op='missing'),
  function(x, cols, vals, comps)
    return(mwhich.internal(x, cols, vals, comps, op='OR', 
      whichFuncName='MWhichBigMatrix')))

setMethod('mwhich',
  signature(x='matrix', op='missing'),
  function(x, cols, vals, comps)
  {
    if (is.integer(x))
      return(mwhich.internal(x, cols, vals, comps, op='OR', 
        whichFuncName='MWhichRIntMatrix'))
    if (is.numeric(x))
      return(mwhich.internal(x, cols, vals, comps, op='OR', 
        whichFuncName='MWhichRNumericMatrix'))
    stop("Unsupported matrix type given to mwhich")
  })

mwhich.internal <- function(x, cols, vals, comps, op, whichFuncName) 
{
  if (is.character(cols)) cols <- mmap(cols, colnames.bm(x))
  if (is.shared(x) && options()$rlock.enabled) lockcols(x, cols, 'r')
  if (length(setdiff(cols, 1:ncol(x))) > 0)
    stop('Invalid column(s) in which()')

  # if vals or comps are not lists but are length 1 or 2, make them
  # trivial lists.
  if ( !is.list(vals) & 
       (length(vals)==1 || length(vals)==2) ) {
    vals <- list(vals)
    #warning('coerced vals to list in which.bm')
  } else {
    if (!is.list(vals)) stop('vals should be a list')
  }
  if ( !is.list(comps) &
       (length(comps)==1 || length(comps)==2)) {
    comps <- list(comps)
    #warning('coerced comps to list in which.bm')
  } else {
    if (!is.list(comps)) stop('comps should be a list')
  }

  # Replicate vals or comps if appropriate.
  if (length(cols)!=length(vals)) {
    if (length(vals)==1) {
      vals <- data.frame(matrix(unlist(vals), length(vals), length(cols)))
    } else stop('length(vals) must be 1 or length(cols)')
  }
  if (length(cols)!=length(comps)) {
    if (length(comps)==1) {
      comps <- data.frame(matrix(unlist(comps), length(comps), length(cols)),
                          stringsAsFactors=FALSE)
    } else stop('length(comps) must be 1 or length(cols)')
  }
  if (length(comps)!=length(vals)) stop('length of comps must equal length of vals')

  testCol <- cols
  opVal <- 0
  if (op == 'OR') opVal <- 1
  minVal <- rep(NA, length(cols))
  maxVal <- rep(NA, length(cols))
  chkmin <- rep(0, length(cols))
  chkmax <- rep(0, length(cols))

  if (any(!unlist(lapply(comps, is.character))) ||
      any(!(unlist(comps) %in% c('eq', 'neq', 'le', 'lt', 'ge', 'gt')))) {
    stop('comps must contain eq, neq, le, lt, ge, or gt')
  }
  for (i in 1:length(cols)) {

    if (length(vals[[i]])==1) {
      # Here, we have the easy comparisons.
      if (is.na(vals[[i]]) && (comps[[i]]!='eq' && comps[[i]]!='neq'))
        stop('NA comparison limited to eq and neq, not le, lt, gt, or ge')
      if (length(comps[[i]])==1) {
        if (comps[[i]]=='eq' || comps[[i]]=='neq') {
          minVal[i] <- vals[[i]]
          maxVal[i] <- vals[[i]]
        }
        if (comps[[i]]=='neq') {
          chkmin[i] <- -1
          chkmax[i] <- -1            # Not used, but....
        }
        if (comps[[i]]=='ge' || comps[[i]]=='gt') {
          minVal[i] <- vals[[i]]
          maxVal[i] <- Inf
          if (comps[[i]]=='gt') chkmin[i] <- 1
        }
        if (comps[[i]]=='le' || comps[[i]]=='lt') {
          minVal[i] <- -Inf
          maxVal[i] <- vals[[i]]
          if (comps[[i]]=='lt') chkmax[i] <- 1
        }
      } else stop('vals/comps must be componentwise of same dimension')
    } else {
      # Here, we have two vals and two comps
      if (any(is.na(vals[[i]]))) stop('NAs not allowed in interval comparison')
      minVal[i] <- vals[[i]][1]
      maxVal[i] <- vals[[i]][2]
      if (comps[[i]][1]=='gt') chkmin[i] <- 1
      if (comps[[i]][2]=='lt') chkmax[i] <- 1
      if (comps[[i]][1]!='gt' && comps[[i]][1]!='ge')
        stop('invalid comparison of lower bound')
      if (comps[[i]][2]!='lt' && comps[[i]][2]!='le')
        stop('invalid comparison of upper bound')
    }

  } # End of the for loop

##### The new C function has new vectors chkmin and chkmax;
##### the value 0 indicates comparison with equality,
##### the value 1 indicates a strict inequality,
##### the value -1 indicates a 'neq' check;
##### if is.na checking is required, only the minVal needs to be
##### used, with chkmin = 0 being is.na and chkmin = 1 being !is.na.

  ret = NULL
  if (whichFuncName == 'MWhichBigMatrix')
    ret = .Call(whichFuncName, x@address, as.double(testCol), 
                as.double(minVal), as.double(maxVal), 
                as.integer(chkmin), as.integer(chkmax), as.integer(opVal))
  else
    ret = .Call(whichFuncName, x, nrow(x),
                as.double(testCol), 
                as.double(minVal), as.double(maxVal), 
                as.integer(chkmin), as.integer(chkmax), as.integer(opVal))

  if (is.shared(x) && options()$rlock.enabled)
    unlockcols(x, cols)

  return(ret)
}

rm.cols <- function(x, remove.columns)
{
  if (is.shared(x)) stop("unable to remove columns of a shared matrix.")
  if (is.separated(x)) stop("unable to remove columns of a separated matrix.")

  if (length(remove.columns) != unique(length(remove.columns))) 
    stop("You can only remove a column once.")
  if (is.character(remove.columns)) 
    remove.columns= mmap(remove.columns, colnames(x))
  else if (min(remove.columns)<1 | max(remove.columns)>ncol(x)) 
    stop("Error: column indices out of range.")
  remove.columns = sort(remove.columns, decreasing=TRUE)
  for (rmCol in remove.columns) {
    .Call('CEraseMatrixCol', x@address, as.double(rmCol))
    # TODO: For some reason, if I don't put a statement after this call
    # I get a segfault from R (according to valgrind).  
    i=1 
  }
}

add.cols <- function(x, ncol=1, init=0, column.names=NULL)
{
  if (is.shared(x)) stop("unable to add columns to a shared matrix.")
  if (is.separated(x)) stop("unable to add columns to a separated matrix.")

  if (ncol < 1) stop('You must add at least 1 column.')
  oldncols = ncol(x)
  if (is.null(column.names) && !is.null(colnames(x))) {
    column.names <- paste(rep('V', ncol), 
                          (oldncols+1):(ncol(x)+ncol), sep=".")
  }
  if (length(unique(c(colnames.bm(x), column.names))) != ncol(x)+ncol)
    stop("Variable names conflict in add.cols.\n")
  for (i in 1:ncol) {
    .Call('CAddMatrixCol', x@address, as.double(init))
  }
  if (!is.null(colnames(x))) colnames(x)[-c(1:oldncols)] = column.names
}

setMethod('dimnames', signature(x = "big.matrix"),
  function(x) return(list(rownames.bm(x), colnames.bm(x))))

setMethod('dimnames<-', signature(x = "big.matrix", value='list'),
  function(x, value) {
    rownames.bm(x) = value[[1]]
    colnames.bm(x) = value[[2]]
    return(x)
  })

hash.mat <- function(x, col)
{
  if (colmin(x, col)<1) 
    stop("Error: minimum value in specified column should be 1 or more.")
  return(matrix(.Call('MatrixHashRanges', x@address, as.double(col)),
                      ncol=2, byrow=TRUE))
}

read.big.matrix <- function(fileName, sep=',', header=FALSE, row.names=NULL, 
  col.names=NULL, type=NA, skip=0, separated=FALSE,  shared=FALSE,
  backingfile=NULL, backingpath=NULL, preserve=TRUE) 
{ stop("Error: You must specify a file name.") }

setMethod('read.big.matrix', signature(fileName='character'),
  function(fileName, sep=',', header=FALSE, row.names=NULL, col.names=NULL,
           type=NA, skip=0, separated=FALSE, shared=FALSE, 
           backingfile=NULL, backingpath=NULL, preserve=TRUE)
  {
    headerOffset <- as.numeric(header)
    colNames=NULL
    if (header) {
      colNames = unlist(strsplit(
        scan(fileName, what='character', skip=skip, nlines=1, sep="\n", 
          quiet=TRUE), split=sep))
      if (is.character(col.names)) {
        warning("Using supplied column names and skipping the header row.\n")
        colNames <- col.names
      } else {
        if (!is.null(col.names))
          stop("You need to decide between column names and a header.\n")
      }
    } else {
      if (is.character(col.names)) colNames <- col.names
    }
		# Get the first line of data
    firstLineVals <- unlist(strsplit(
      scan(fileName, what='character', skip=(skip+headerOffset), 
           nlines=1, sep="\n", quiet=TRUE), split=sep))

		# See if there are row names (a row name always has a double quote)
		rowNames=NULL
		userowNames=FALSE
		hasQuoteInFirstLine = grep( '"', firstLineVals )
		hasRowNames = length(hasQuoteInFirstLine) > 0 && hasQuoteInFirstLine > 0
		if (hasRowNames)
		{
			userowNames=TRUE
		}
    
    if (length(colNames) == length(firstLineVals)) {
      # No row names, may or may not be column names, but we don't care
      # and nothing more needs to be done at this point.
    }
    if (!is.null(colNames) && (length(colNames)==length(firstLineVals))) {
      # Column and row names both exist
      if (is.logical(row.names) && !row.names) 
        stop("Error: row names seem to exist in this file.\n")
      if (is.logical(row.names) && row.names) {
        firstLineVals = firstLineVals[-1]
        userowNames <- TRUE
      }
      if (is.character(row.names)) {
        warning("Using supplied row names instead of those in the file.\n")
        rowNames <- row.names
        userowNames <- FALSE
      }
    } 
    if (is.null(colNames) && is.logical(row.names)) {
      if (row.names) {
        firstLineVals = firstLineVals[-1]
        userowNames <- TRUE
      }
    }

    numCols <- length(firstLineVals) - as.integer(hasRowNames)
    if (is.na(type)) {
      warning('big.matrix type was not specified, going by first line, noting that a choice will be made between double and integer only (not short or char).')
      type <- 'double'
      if (sum(na.omit(as.integer(firstLineVals)) ==
              na.omit(as.double(firstLineVals))) ==
        numCols ) 
      {
        type <- 'integer'
      }
    }

    lineCount <- .Call("CCountLines", fileName) - skip - headerOffset
    numRows <- lineCount
    if (!shared & is.null(backingfile))
    {
      bigMat = big.matrix(nrow=numRows, ncol=numCols, type=type, 
        dimnames=list(rowNames, colNames), init=0)
    } 
    else 
    {
      if (!is.null(backingfile)) 
      {
        bigMat = filebacked.big.matrix(nrow=numRows, ncol=numCols, type=type,
          dimnames=list(rowNames, colNames), init=NULL, separated=separated,
          backingfile=backingfile, backingpath=backingpath, preserve=preserve)
      } 
      else 
      {
        bigMat = shared.big.matrix(nrow=numRows, ncol=numCols, type=type, 
          dimnames=list(rowNames, colNames), init=NULL, separated=separated )
      }
    }
    ############################################################
    # if userowNames == NULL, then there aren't any in the file.
    # if userowNames == TRUE, then they exist: use them!
    # if userowNames == FALSE, then they exist, but you ignore them.
    .Call('ReadMatrix', fileName, bigMat@address, 
          as.integer(skip+headerOffset), as.integer(numRows), 
          as.integer(numCols), sep, hasRowNames, userowNames)
#    if (is.logical(userowNames) && userowNames == FALSE)
#      rownames(bigMat) = row.names
    return(bigMat)
  })

write.big.matrix <- function(x, fileName=NA, row.names = FALSE, col.names = FALSE,
  sep=",")
{ stop("Error: You must specify a bigmatRix and file name.") }

setMethod('write.big.matrix', signature(x='big.matrix',fileName='character'),
function(x, fileName, row.names = FALSE, col.names = FALSE, sep=",")
{
  if (is.character(row.names)) stop("You must set the row names before writing.\n")
  if (is.character(col.names)) stop("You must set the column names before writing.\n")
  if (row.names & !.Call("HasRowColNames",x@address)[1]) {
    row.names <- FALSE
    warning("No row names exist, overriding your row.names option.\n")
  }
  if (col.names & !.Call("HasRowColNames",x@address)[2]) {
    col.names <- FALSE
    warning("No column names exist, overriding your col.names option.\n")
  }

  .Call('WriteMatrix', x@address, fileName, as.logical(row.names), 
        as.logical(col.names), sep)
  invisible(NULL)
})

setGeneric('is.shared', function(x) standardGeneric('is.shared'))

setMethod('is.shared', signature(x='big.matrix'),
  function(x)
  {
    return(.Call("IsShared", x@address))
  })

setMethod('is.shared', signature(x='matrix'), function(x) return(FALSE))

setGeneric('is.separated', function(x) standardGeneric('is.separated'))

setMethod('is.separated', signature(x='big.matrix'),
  function(x)
  {
    return(.Call("IsSeparated", x@address))
  })

setMethod('is.separated', signature(x='matrix'), function(x) return(FALSE))

deepcopy <- function(x, type=NULL, separated=NULL, shared=NULL, 
	backingfile=NULL, backingpath=NULL, preserve=TRUE) 
{
	if (nrow(x) > 2^31-1)
	{
		stop(paste("Too many rows to copy for bigmemory version 3.2. ",
			"This will be fixed in the next iteration."))
	}
	if (is.null(type))
	{
		type = typeof(x)
	}
	if (is.null(separated))
	{
		separated=is.separated(x)
	}
	if (is.null(shared))
	{
		shared = is.shared(x)
	}
	y = big.matrix( nrow=nrow(x), ncol=ncol(x), type=type, init=NULL,
		dimnames = dimnames(x), separated=separated, shared=shared,
		backingfile=backingfile, backingpath=backingpath, preserve=preserve)
  for (i in 1:ncol(x))
  {
    y[,i] <- x[,i]
  }
  return(y)
}
