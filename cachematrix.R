## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix <<- inverse
  getinverse <- function() matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  matrix <- x$getinverse()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setinverse(matrix)
  matrix
}

a = makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 1, 1, 1), nrow=3, ncol=3))

aInv = cacheSolve(b)
aInv
aInvCached = cacheSolve(b)
aInvCached

b = makeCacheMatrix(matrix(c(5, 4, 2, 5, 5, 7, 2, 2, 2), nrow=3, ncol=3))

bInv = cacheSolve(b)
bInv
bInvCached = cacheSolve(b)
bInvCached