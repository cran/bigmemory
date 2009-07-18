
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

# MJK 7/12
# If mmap is not export in bigmemory, then this function will need to be
# uncommented when this file is moved to the bigmemoryAnalytics 
# package.
#mmap = function(x, y) 
#{
#  if (is.null(x)) return(NULL)
#  ans <- match(x, y) #y %in% x
#  if (any(is.na(ans))) stop("Couldn't find a match to one of the arguments.")
#  return(ans)
  #if (length(ans) == length(x)) return(ans)
  #else stop("Couldn't find a match to one of the arguments.")
#}


chunkGenerator=function(chunkSize, lastIndex)
{
  .chunkSize=chunkSize
  .currentIndex=1
  .lastIndex=lastIndex
  function(reset)
  {
    if (reset==TRUE)
    {
      .currentIndex <<- 1
      return(NULL)
    }
    if (.currentIndex > .lastIndex)
    {
      return(NULL)
    }
    else
    {
      newLast = min(.currentIndex+.chunkSize-1,lastIndex)
      ret = .currentIndex:newLast
      .currentIndex <<- newLast+1
      return(ret)
    }
  }
}

dataFrameGenerator=function(data, cols, levelList, getNextChunk)
{
  .cols=col
  .data=data
  .levelList = levelList
  .getNextChunk = getNextChunk
  function(reset)
  {
    nextIndices = .getNextChunk(reset)
    if (is.null(nextIndices))
    {
      return(NULL)
    }
    df = as.data.frame(data[nextIndices, cols])
    if (!is.null(.levelList))
    {
      for (name in names(.levelList))
      {
        df[,name] = factor( df[,name], levels=.levelList[[name]] )
      }
    }
    return(df)
  }
}

CreateNextDataFrameGenerator = function( formula, data, chunksize, fc, 
  getNextChunkFunc )
{
  if (!is.null(chunksize) && is.null(getNextChunkFunc))
  {
    # Create the chunk generator
    getNextChunkFunc = chunkGenerator(chunksize, nrow(data))
  }
  if (is.null(chunksize) && is.null(getNextChunkFunc))
  {
    chunksize = floor(nrow(data)/ncol(data)^2)
    getNextChunkFunc = chunkGenerator(chunksize, nrow(data))
  }
  levelList=NA
  if (!is.null(fc))
  {
    levelList = list()
    {
      for (i in 1:length(fc))
      {
        levelList = append( levelList,
          list( sort( unique( as.numeric(data[,fc[i]])) ) ) )
      }
    }
    names(levelList)=fc
  }
  vars = GetVarsFromFormula(formula, colnames(data))
  cols = mmap(vars, colnames(data))
  return(dataFrameGenerator(data, cols, levelList, getNextChunkFunc))
}

bigglm.big.matrix = function( formula, data, chunksize=NULL, ..., fc=NULL,
  getNextChunkFunc=NULL)
{
  require('biglm')
  getNextDataFrame = CreateNextDataFrameGenerator(formula, data, 
    chunksize, fc, getNextChunkFunc)
  return(bigglm(formula=formula, data=getNextDataFrame, chunksize=chunksize,
    ... ))
}


biglm.big.matrix = function( formula, data, chunksize=NULL, ..., fc=NULL,
  getNextChunkFunc=NULL)
{
  require('biglm')
  getNextDataFrame = CreateNextDataFrameGenerator(formula, data,
    chunksize, fc, getNextChunkFunc)
  data = getNextDataFrame(FALSE)
  blm = biglm(formula=formula, data=data, ...)
  data = getNextDataFrame(FALSE)
  while(!is.null(data))
  {
    blm = update(blm, data)
    data = getNextDataFrame(FALSE)
  }
  return(blm)
}

