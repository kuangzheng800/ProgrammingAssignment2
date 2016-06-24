## makeMatrix functions as a generator of the matrix data structure with cached inverse 
## inverse function was defined within the structure

f = makeMatrix(matrix(c(1,3,5,13,17,19,2,2,2),ncol = 3))

makeMatrix = function(x)
{
  r = NA;
  set = function(y)
  {
    x <<- y;
    r <<- NA;
  }
  getmatrix = function() x
  getinverse = function() r
  setinverse = function() r <<- solve(x)   #defined the inverse function within the making function for integrity purpose
  list(set = set, matrix = getmatrix,
       setinverse = setinverse,
       getinverse  = getinverse)
}

## cachesolve interogates the inverse of the matrix structure, and cahes the inverse info on the background if such not yet done. 

cachesolve = function(x)
{
  r = x$getinverse()
  if (!is.na(r))
  {
    message("getting cached data")
    return(r)
  }
  
  x$setinverse()
  r = x$getinverse()
  r
}

