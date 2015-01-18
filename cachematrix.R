## Functions that effciently calculate an inverse of matrix. 
## When invoked the first time, the makeCacheMatrix will cache 
## the value. The cacheSolve function checks the cache and if 
## object is present, the value is returned from cache thus
## saving computational resources.

## Example usage:
## x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## cacheSolve(x)
## if cacheSolve(x) is run repeatedly, 
## from the second run onwards, you should see "getting cached data"
## along with the result.

## Function to cache the matrix (input) and inverse of matrix (output/result)

makeCacheMatrix <- function(x = matrix()) {
	## Initialize result.
    imat <- NULL

    ## Setter to set input matrix as supplied
    ## and initialize result.
    set <- function(y) {
            x <<- y
            imat <<- NULL
    }

    ## Getter to get the input matrix.
    get <- function() x

    ## Setter function to set inverse matrix supplied
    ## and cache to enviroment for later use.
    setinv <- function(invmat) imat <<- invmat

    ## Getter function to return the cached value.
    getinv <- function() imat

    ## Methods
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function to calculate inverse of matrix and cache it for future use

cacheSolve <- function(x, ...) {
   
   ## Retrieve inverse matrix from cache.
   imat <- x$getinv()

   ## If the value is non null i.e. valid object.
   if(!is.null(imat)) {
	  message("getting cached data")
	  return(imat)
   }

   ## No cached value exists. 
   ## Retrieve input matrix
   data <- x$get()

   ## Calculate inverse matrix,
   ## assuming a square invertible matrix.
   ## As per assignment, this is a valid
   ## assumption.
   imat <-solve(data, ...)

   ## Setter function to cache the value.
   x$setinv(imat)

   ## Return the value.
   imat       
}
