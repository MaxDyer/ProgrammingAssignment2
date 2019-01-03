## Sets the value of the matrix, gets the value of the matrix, sets the value of the inverse,
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
  set <- function(y) {
          x <<-y
          v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    v <- x$getInverse()
    if (!is.null(v)) {
        message ("getting cached data")
        return(v)
    }
    data <-x$get()
    v <- inverse(data, ...)
    x$setInverse(v)
    v
}
