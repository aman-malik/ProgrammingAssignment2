

#### Caching the Inverse of a matrix!

#### This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#### This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
#### If the inverse has already been calculated (without the matrix changed), 
#### then it should give gthe inverse from cache.

cachesolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


#### Test the function

my_matrix <- makeCacheMatrix(matrix(1:4, 2,2))
my_matrix$get()
my_matrix$getInverse()
cachesolve(my_matrix)
