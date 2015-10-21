## This set of functions allows you to avoid repeating a matrix inverse calculation
## by caching the inverse with the original matrix when it is first calculated


##Function for creating a matrix object with a cached inverse

makeCacheMatrix <- function(x = matrix()) {           
      inv <- NULL                                     ##Initialise the cahce inverse variable as NULL
      set <- function(y) {                            ##Function for setting the matrix
            x <<- y                                   ##set the 'x' variable to the matrix passed to the 'set()' function
            inv <<- NULL                              ##reset the 'inv' variable to NULL as the new matrix may have a new inverse
      }
      get <- function() x                             ##Function for retrieving the matrix - returns the 'x' variable
      setinv <- function(inverse) inv <<- inverse     ##Function for setting the cached inverse - sets 'inv' variable to the matrix passed to it
      getinv <- function() inv                        ##Function for retrieving the cached inverse - returns the 'inv' variable
      list(set = set, get = get,                      ##Returns a list of all the contained functions when 'makeCacheMatrix' is called
           setinv = setinv,
           getinv = getinv)
}


## Function for calculating inverse of a matrix or returning cached inverse if available

cacheSolve <- function(x, ...) {               
      inv <- x$getinv()                        ##call the getinv() function from an object 'x' created from the 'makeCacheMatrix()' function to see if it contains a cached inverse
      if(!is.null(inv)) {                      ##if 'x' contains a cached inverse then return the cached inverse matrix
            message("getting cached data")
            return(inv)
      }
      data <- x$get()                          ##if 'x' does not contain a cached inverse then get the matrix stored in the 'x' object
      inv <- solve(data)                       ##calculate the inverse of the matrix
      x$setinv(inv)                            ##call the 'setinv()' function for the object 'x' and cache the calculated inverse in the object for later retrieval
      inv                                      ##return the calculated inverse
}
