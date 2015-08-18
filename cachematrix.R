## These functions will cache the inverse of a given matrix.

## makeCacheMatrix creates the matrix that will ultimately cache the inverse.

makeCacheMatrix <- function(x = matrix()) 
{
      invert <- NULL
      set <- function(c)
      {
            x <<- c
            inv <<- NULL
        
      }
        
      get <- function() x
      setMean <- function(calculateMean) calculateMean <<- calculateMean
      getMean <- function() calculateMean
      list(set = set, get = get, setMean = setMean, getMean = getMean)
}


## This function takes the matrix from the makeCacheMatrix function
## and will compute the inverse.

cacheSolve <- function(x, ...) 
{
      ## Return a matrix that is the inverse of 'x'
      invert <- x$getMean()
      if(!is.null(invert))
      {
          message("getting cached data")
          return(invert)
      }
      cache <- x$get()
      invert <- calculateMean(cache, ...)
      x$setMean(invert)
      invert
}
