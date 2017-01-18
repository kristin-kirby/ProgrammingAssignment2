## This pair of functions calculate and cache the inverse of an invertible matrix. 
## If the matrix fed to the functions is not invertible, the functions return a message that 
## the matrix is not invertible. 

## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      
      ## return character string that matrix is not invertible if it is not invertible
      if (class(try(solve(x),silent=T))=="try-error") {
            errormessage <- "Matrix is not invertible"
            return(errormessage)
      }
     
       ##  if matrix is invertible, continue
      else {
            inv <- NULL 
            set <- function(y) {
                  x <<- y 
                  inv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            
            list(set = set, get = get, setinv = setinv, getinv = getinv)
      }
}


##The cacheSolve function checks to the see if the inverse of a particular matrix has
## already been calculated. If it has, the function returns the cached inverse. Otherwise, 
## the inverse is calculated. 
cacheSolve <- function(x, ...) {
      if (class(x)=="character") {
            message(x)
      }
      else {
            inv <- x$getinv()
            if (!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
            }
            
            data <- x$get()
            inv <- solve(data,...)
            x$setinv(inv)
            inv
      }
}
