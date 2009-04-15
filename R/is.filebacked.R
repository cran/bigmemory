setGeneric('is.filebacked', function(x) standardGeneric('is.filebacked'))

setMethod('is.filebacked', signature(x='big.matrix'),
  function(x)
  {
    return(.Call("IsFileBackedBigMatrix", x@address))
  })

setMethod('is.filebacked', signature(x='matrix'), function(x) return(FALSE))

