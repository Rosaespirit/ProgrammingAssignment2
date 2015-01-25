## The 2 functions are meant to work together in order to perform actions on a matrix passed to 
## the first one ("makeCacheMatrix"). "makeCacheMatrix" will set variables meant to cache 
## the inverse of this matrix, (calculated by calling "cacheSolve") and define functions to get or
## set these variables."cacheSolve" will check if a value already exists where 
## the inverted matrix should be cached and return it, otherwise the function will calculate it 
## and return it.

## Here "a" will cache the inverse of the matrix "x" passed to the function, while "b" is a logical
## vector used to verify if the value of "x" has been changed, as defined in the function "set".
## The other functions inserted in the list at the end will return "x" ("get"), 
## set its inverse ("setInvMatrix"), get this inverse "stored" in "a" ("getInvMatrix") or
## return b ("xChangedTrue"). I chose not to flush the cache when x is changed, to let the user
## a chance to retrieve the value of the previous inverted matrix before
## the next call of "cacheSolve", which will reset "b" to FALSE (x changed).

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  b <- FALSE
  setA <- function(y)
  {
    if ((x <<- y) & (x != y))
    {  
      x <<- y
      b <- TRUE
      message("Call cacheSolve again, value of matrix passed to makeCacheMatrix has changed")
    }
  } 
  get <- function() x
  setInvMatrix <- function(solve) a <<- solve
  getInvMatrix <- function() a
  xChangedTrue <- function() b
  resetxChangedTrue <- function(set) b <<- set
  list(setA = setA, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix,
       xChangedTrue = xChangedTrue, resetxChangedTrue = resetxChangedTrue)
}


## This function first retrieve the value of the variable in "makeCacheMatrix()" used to cache 
## the inverted matrix. The cached data will only be returned if this value is not null and
## if the value of the matrix previously passed to "makeCacheMatrix()" hasn't changed since last
## call of "cacheSolve()". Otherwise, this variable ("a") will be overwritten by a new calculation
## of matrix inverse and its value returned.

cacheSolve <- function(x, ...)
{
  a <- x$getInvMatrix()
  b <- x$xChangedTrue()
  data <- x$get()
  if((!is.null(a)) & (b = FALSE))
  {
    message("getting cached data")
    return(a)
  }
  else 
  {
    a <- solve(data, ...)
    x$setInvMatrix(a)
    x$resetxChangedTrue(FALSE)  ## The inverted matrix corresponds again to the last matrix
    a                           ## passed to "makeCacheMatrix", we can reset b to FALSE in
  }                             ## "makeCacheMatrix"
}
