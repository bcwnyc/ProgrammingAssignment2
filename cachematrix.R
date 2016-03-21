## BCW's ProgrammingAssignment2

## 'makeCaheMatrix' creates a matrix and then its inverse matrix


makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}



## 'cacheSolve'calculates the inverse of the matrix from 'makeCaheMatrix'
## If the matrix is the same, this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) 
{
  
  inv <- x$getInverse()
  if (!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
