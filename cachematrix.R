## program the calculate and cache the inverse of a matrix
## it is assumed that that the argument is always a matrix and is invertible

## makeCacheMatrix creates an object to catch the inverse of the given matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL

  set <- function(y)
  {
	x <<- y
	inv <<- NULL
  }

  getmatrix <- function() x

  setinverse <- function(inverse) inv <<- inverse
 
  getinverse <- function() inv

  list(set = set, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix (if not already cached in the object created by makeCacheMatrix)
cacheSolve <- function(x,...) 
{
  ## Return the inverse of x

  ## if inverse already exists in the cache then return it
  
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)

  ##solve for the inverse 

  data <- x$getmatrix()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
