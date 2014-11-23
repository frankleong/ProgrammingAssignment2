## Set of functions that calculate the inverse of a matrix, and cache it
##   for next call so that the same calculation does not need to perform again.

## If the inverse of a matrix is calculated and being asked again, system
##   will get it from memory instead of calculate it again. It can help to save
##   the system resource.

## Function makeCacheMatrix
##   For creation of an object which keep a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL          ## Initial the result variable.
    
    set <- function(y) {          ## Assign new value to object, and reset result.
        x <<- y
        m <<- NULL
    }
    
    get <- function() {           ## Return the original matrix.
        x 
    }
    
    setinv <- function(solve) {           ## Cache the result inverse of matrix. 
        m <<- solve
    }
    
    getinv <- function() {           ## Return the cache result.
        m
    }
    
    list(set = set, get = get,          ## A list storing the result of function.
         setinv = setinv,
         getinv = getinv)
}


## Function cachSolve
##    Return a matrix that is the inverse of input matrix 'x'.
##    If the inverse of input value is calculated before, the result will be
##      retreived from cache, and no calculation will perform again.

cacheSolve <- function(x, ...) {
  
    m <- x$getinv()          ## Try to get the cache value.
    
    if(!is.null(m)) {          ## If cache value exist, return it and end function.
      message("getting cached data")
      return(m)
    }

    ## If cache value not found, continue function.
    data <- x$get()          ## Assign the input matrix for calculation.
    
    m <- solve(data, ...)          ## Calculate the inverse of matrix.
    
    x$setinv(m)          ## Cache the result in calculation.
    
    m           ## Return the calculated result.
}

