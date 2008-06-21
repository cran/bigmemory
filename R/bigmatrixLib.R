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


setGeneric('colmin', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colmin'))

setMethod('colmin', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    thistype = .Call("CGetType", x@address)
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    ret = .Call("CMinColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("min", signature="big.matrix",
  function(x, ..., na.rm=FALSE) {
    return(min(colmin(x, ..., na.rm=na.rm)))
  })

setGeneric('colmax', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colmax'))

setMethod('colmax', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    thistype = .Call("CGetType", x@address)
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    ret = .Call("CMaxColmain", as.integer(thistype), 
      x@address, as.double(cols), na.rm)

    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("max", signature="big.matrix",
  function(x, ..., na.rm=FALSE)
  {
    return(max(colmax(x, ..., na.rm=na.rm)))
  })

setGeneric('colprod', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colprod'))

setMethod('colprod', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = .Call("CGetType", x@address)
    ret = .Call("CProdColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("prod", signature="big.matrix",
  function(x, ..., na.rm=FALSE) {
    return(prod(colprod(x, ..., na.rm=na.rm)))
  })

setGeneric('colsum', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colsum'))

setMethod('colsum', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = .Call("CGetType", x@address)
    ret = .Call("CSumColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("sum", signature="big.matrix",
  function(x, ..., na.rm=FALSE) {
    return(sum(colsum(x, ..., na.rm=na.rm)))
  })


setGeneric('colrange', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colrange'))

setMethod('colrange', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) {
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    #if (length(cols)==1) 
    #  return(c(colmin(x,cols=cols,na.rm=na.rm),
    #           colmax(x,cols=cols,na.rm=na.rm)))
    ret = matrix(c(colmin(x,cols=cols,na.rm=na.rm), 
      colmax(x,cols=cols,na.rm=na.rm)), ncol=2)
    colnames(ret) = c('min', 'max')
    if (!is.null(colnames(x))) rownames(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod("range", signature="big.matrix",
  function(x, ..., na.rm=FALSE)
  {
    #cl <- match.call()
    #if (any(names(cl)=="cols")) 
    #{
    #  args = list(...)
    #  cols = args$cols
    #  return(colrange(x, cols, na.rm=na.rm))
    #}
    rangeMat = colrange(x, ..., na.rm=na.rm)
    return(c(min(rangeMat[,1]), max(rangeMat[,2])))
  })

setGeneric('colmean', function(x, cols=NULL, na.rm=FALSE) 
  standardGeneric('colmean'))

setMethod('colmean', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE) 
  {
    if (is.null(cols)) cols=1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = .Call("CGetType", x@address)
    ret = .Call("CMeanColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod('mean', signature(x="big.matrix"),
  function(x, ...)
  {
    #cl = list(...)
    #colArgIdx = which( names(cl) == 'cols' )
    #if (length(colArgIdx) == 0)
    #  cols = 1:ncol(x)
    #else
    #  cols = cl$colArgIdx
    #naRmIdx = which(names(cl) == 'na.rm')
    #if (length(naRmIdx) == 0)
    #  na.rm=FALSE
    #else
    #  na.rm = cl$na.rm
    return(mean(colmean(x, ...)))
  })

setGeneric('colvar', function(x, cols=NULL, na.rm=FALSE) 
  standardGeneric('colvar'))

setMethod('colvar', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE)
  {
    if (!is.big.matrix(x)) stop("Unknown type.")
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    thistype = .Call("CGetType", x@address)
    ret = .Call("CVarColmain", as.integer(thistype), x@address, 
      as.double(cols), na.rm)
    if (!is.null(colnames(x))) 
      names(ret) = colnames(x)[cols]
    return(ret)
  })

setGeneric('colsd', function(x, cols=NULL, na.rm=FALSE)
  standardGeneric('colsd'))

setMethod('colsd', signature(x='big.matrix'),
  function(x, cols=NULL, na.rm=FALSE)
  {
    return(sqrt(colvar(x, cols=cols, na.rm=na.rm)))
  })

setGeneric('ColCountNA', function(x, cols=NULL) standardGeneric('ColCountNA'))

setMethod('ColCountNA', signature(x='big.matrix'),
  function(x, cols=NULL)
  {
    if (is.null(cols)) cols = 1:ncol(x)
    if (is.character(cols)) cols <- mmap(cols, colnames(x))
    cols = as.double(cols)
    if (max(cols) > ncol(x) | min(cols) < 1) stop("Invalid columns")
    ret = c()
    for (col in cols) ret = c(ret, .Call('ColCountNA', x@address, col))
    if (!is.null(colnames(x))) names(ret) = colnames(x)[cols]
    return(ret)
  })

setMethod('summary',
  signature(object='big.matrix'),
  function(object)
  {
    rows = 1:ncol(object)
    cn = c('min', 'max', 'mean', "NAs")
    s = matrix(NA, ncol = length(cn), nrow = length(rows))
    colnames(s) = cn
    rownames(s) = colnames(object)
    s[,'min'] = colmin(object, rows, na.rm=TRUE)
    s[,'max'] = colmax(object, rows, na.rm=TRUE)
    s[,'mean'] = colmean(object, rows, na.rm=TRUE)
    s[,"NAs"] = ColCountNA(object, rows)
    return(data.frame(s))
  })

GetVarsFromFormula = function(formula, cn)
{
  cForm = as.character(formula)
  vars = c()
  for (i in 1:length(cn))
  {
    notVarString = "((^)|([^\\^_\\.0-9A-Za-z$])|($))" 

    if (regexpr(paste(notVarString, cn[i], notVarString, sep=''), 
      cForm[3], perl=TRUE)[1] > 0)
    {
      vars = c(vars, cn[i])
    }
    if (regexpr(paste(notVarString, cn[i], notVarString, sep=''), cForm[2],
      perl=TRUE) > 0)
    {
      vars = c(vars, cn[i])
    }
  }
  return(vars)
}


# Should we be calling gc() at the end of updates?
# fc should be the names of the factor variables in the formula.
biglm.big.matrix = function(formula, data, fc=NULL, chunksize=NULL, 
  weights=NULL, sandwich=FALSE)
{
  if (require('biglm')==FALSE)
    stop('biglm package not installed.')
  if (is.null(chunksize)) chunksize = floor(nrow(data)/ncol(data)^2)
  chunkSeq = seq(1, nrow(data), chunksize)
  lm.bm = NULL
  vars = GetVarsFromFormula(formula, colnames(data))
  factorList = list()
  if (length(fc) > 0) {
    for (i in 1:(length(fc))) 
      factorList = append(factorList, list(unique(as.numeric(data[,fc[i]]))))
  }
  names(factorList) = fc
  for (i in 1:(length(chunkSeq)-1)) {
    begChunk = chunkSeq[i]
    endChunk = begChunk+chunksize-1
    if (i == length(chunkSeq)-1)  endChunk = nrow(data)
    # Is this a good idea?  Should the dataframe be made from scratch?
    s = paste('data.frame(',vars[1],'=as.numeric(data[begChunk:endChunk,"', 
      vars[1],'"]))', sep='')
    d = eval(parse(text=s))
    for (j in 2:length(vars)) {
      s = paste('d$',vars[j],'=as.numeric(data[begChunk:endChunk,"', 
        vars[j],'"])', sep='')
      eval(parse(text=s))
    }
    for (n in fc) 
      d[,n] = factor(d[,n], factorList[[n]])
    if (i==1) 
      lm.bm = biglm(formula=formula, data=d, weights=weights, sandwich=sandwich)
    else lm.bm = update(lm.bm, d)
  }
  return(lm.bm)
}

bigglm.big.matrix = function( formula, data, family=gaussian(), fc=NULL, 
  chunksize=NULL, weights=NULL, sandwich=FALSE, maxit=8, tolerance=1e-7,
  start=NULL)
{
  if (require('biglm')==FALSE)
    stop('biglm package not installed.')
  if (is.null(chunksize))
  {
    chunksize = floor(nrow(data)/ncol(data)^2)
  }
  chunkSeq = seq(1, nrow(data), chunksize)
  lm.bm=NULL
  vars = GetVarsFromFormula(formula, colnames(data))
  factorList = list()
  if (length(fc) > 0)
  {
    for (i in 1:(length(fc)))
    {
      factorList = append(factorList, list(unique(as.numeric(data[,fc[i]]))))
    }
  }
  names(factorList) = fc
  for (i in 1:(length(chunkSeq)-1)) {
    begChunk = chunkSeq[i]
    endChunk = begChunk+chunksize-1
    if (i == length(chunkSeq)-1)  endChunk = nrow(data)
    s = paste('data.frame(',vars[1],'=as.numeric(data[begChunk:endChunk,"', 
      vars[1],'"]))', sep='')
    d = eval(parse(text=s))
    for (j in 2:length(vars)) {
      s = paste('d$',vars[j],'=as.numeric(data[begChunk:endChunk,"', 
        vars[j],'"])', sep='')
      eval(parse(text=s))
    }
    for (n in fc) {
      d[,n] = factor(d[,n], factorList[[n]])
    }
    if (i==1) {
      lm.bm = bigglm(formula=formula, data=d, family=family, 
        weights=weights, sandwich=sandwich, maxit=maxit, tolerance=tolerance,
        start=NULL)
    }
    else
      lm.bm = update(lm.bm, d)
  }
  return(lm.bm)
}

