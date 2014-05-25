## makeCacheMatrix returns an object ("special Matrix") ## that contains a matrix, its inverse matrix, 
## and 4 methods to set and get (set, get, setinverse, getinverse)
## ex. makeCacheMatrix(matrix(1:4,2,2))    ====> creates a matrix 2x2, with an null inverse
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


## cacheSolve returns the inverse of a matrix, calculated from data 
## or read from cache.
## ex. cacheSolve(mymatrix) ===> returns an inverse matrix of mymatrix, and also write it in a cache                                
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
