## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix returns a list of functions that will get and set
## the value of the matrix and the value of the matrix inverse

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


## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the special matrix it creates in
## the makeCacheMatrix function. However, before calculating the inverse, it first checks
## to see if the inverse of the special matrix defined above has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
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
