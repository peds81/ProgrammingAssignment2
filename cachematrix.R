## These functions take as an argument a matrix x and return the inverse of that matrix.
## The resultant inverse matrix is stored in the cache to allow fast retrieval. 
##It is assumed that the matrix x is invertible. 

## makeCacheMatrix takes in a matrix x which is assumed to be invertible. It returns
##a special matrix object which when passed to the cacheSolve function will return the
##inverse of x which is stored in the cache. 

makeCacheMatrix <- function(x = matrix()) {
  ##define i here which will eventually hold inverse of x
  i <- NULL
  ##setter function $set stores matrix x in cache and defines i
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##getter function $get returns original data matrix x
  get <- function() x
  ##setter function $setinv caches i - the inverse of the original matrix x
  setinv <- function(inv) i <<- inv
  ##getter function $getinv returns i the inverse of the original matrix x
  getinv <- function() i
  list(get = get, set = set, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a single argument which is the result of a call to the makeCacheMatrix
##function and returns the inverse of the original matrix which was passed to makeCacheMatrix
## storing it in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Check if inverse matrix i is stored in cache and retrieve it if it is. 
  i <- x$getinv()
  if(!is.null(i)){
    message("Getting cached inverse matrix")
    return(i)
  }
  ##Access matrix x using $get function
  mat <- x$get()
  ##Calculate inverse of x
  i <- solve(mat)
  ##Store inverse matrix i in cache
  x$setinv(i)
  i
}
