## makeCacheMatrix is a constructor (O.O.P -like) for a "special" matrix
## that contains a matrix, an inverse matrix, and 4 methods, to set and
## get
##

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m_inv <<- solve
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, calculated or read from
## a cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinverse(m_inv)
  m_inv
}
