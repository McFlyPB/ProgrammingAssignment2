## The 2 functions below allow to compute the inverse of a matrix. The particularity 
## is that the inverse is strored in cache and computed only when needed. So we consider
## here a special "matrix".

## The funciont makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse
##   - get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the invere from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
