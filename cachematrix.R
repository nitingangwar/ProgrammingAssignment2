## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,get = get,setinverse = setinverse, getinverse = getinverse)
}



## The following function cacheSolve calculates the inverse of the special "vector" created with the above function.It also checks if the inverse has already been calculated. If so, it gets the inverse from the cache. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  m
}

