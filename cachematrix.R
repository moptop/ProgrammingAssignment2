## Below are a pair of functions that take a square invertible matrix of numbers, calculate it's inverse
## the first time, then caches and returns the result. On subsequent iterations that these functions take this
## matrix of numbers, its inverse will be retrieved from the cache rather than re-calculated before returning.

## The makeCacheMatrix() function creates 4 sub-functions that can set and get a matrix "x" (and its inverse),
## then returns these 4 functions as one list object. In use, you assign an instance of this function to an object
## with the input matrix as the argument.

makeCacheMatrix <- function(x = matrix()) {
      # initialize the variable i
      i <- NULL
      # Subfunction that sets the input matrix to var "x"
      set <- function(y) {
            # Assign variables to the parent environment
            x <<- y
            i <<- NULL
      }
      # Subfunction that retrieves the input matrix "x"
      get <- function() x
      # Subfunction that sets the inverse matrix to var "i"
      setinverse <- function(inverse) i <<- inverse
      # Subfunction that retrieves the inverse matrix "i"
      getinverse <- function() i
      # Return a list containing all of the Subfunctions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve() function actually computes the inverse of the matrix "x" if it hasn't already been computed and
## cached. The object instance of makeCacheMatrix created becomes a list of the four functions returned from it,
## which in use becomes the argument in this catchSolve function. If the inverse has already been computed, then
## the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      # If i is not NULL, meaning it has already been computed, then return cached matrix
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      # Otherwise get the input matrix
      data <- x$get()
      # And compute the inverse
      i <- solve(data, ...)
      # Either way, now set the inverse matrix
      x$setinverse(i)
      #And return it
      i
}



###############################################################################################
## Running and Testing ########################################################################
###############################################################################################

# a <- makeCacheMatrix(matrix(rnorm(9),3,3))   # an instance of makeCacheMatrix with an input matrix
# cacheSolve(a)   # returns the inverse of the input matrix

###############################################################################################

# a <- makeCacheMatrix()             
# a
# class(a)
# class(a$set)
# a$set(matrix(rnorm(9),3,3))
# a$get()
# cacheSolve(a)
# cacheSolve(a)

