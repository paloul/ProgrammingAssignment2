## Matrix inversion is usually a costly computation and there 
#  may be some benefit to caching the inverse of a matrix rather 
#  than computing it repeatedly. The following functions in this file
#  help compute the inverse of a matrix and cache it for later use.

## The makeCacheMatric function creates a special matrix which really 
#  is a list of containing functions that set/get the value of the actual
#  matrix and set/get the inverse of the matrix.
# 1. set() the value of the matrix
# 2. get() the value of the matrix
# 3. set_inverse() the value of the inverse matrix
# 4. get_inverse() the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # starting off set inv variable to null
  inv <- NULL
  
  # set function that takes a matrix and sets it to internal x matrix
  set <- function(y) {
    x <<- y # set matrix to internal x
    inv <<- NULL # reset the internal inv since the matrix was changed
  }
  
  # get function that returns internal x matrix
  get <- function() x
  
  # given the inverse, sets it to internal inv matrix
  setinverse <- function(new_inv) inv <<- new_inv
  
  # return the internal inv matrix
  getinverse <- function() inv
  
  # create the list of internal functions and return them for use as list
  list(set = set, get = get, set_inverse = setinverse, get_inverse = getinverse)
}


## The next function, cacheSolve, calculates the inverse of a special matrix
#  generated with above function and sets it to the cache with set_inverse.
#  If the inverse is already cached, then it returns the cached value and 
#  skips the recomputation of the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse() # try and get the cache inverse first
  
  # check to see if getting inverse was null or not
  if(!is.null(inv)) {
    message("Inverse was cached. Getting cached inverse.")
    return(inv) # return the retrieved cached inverse
  }
  
  the_matrix <- x$get() # get actual matrix
  inv <- solve(the_matrix) # compute the inverse of matrix
  x$set_inverse(inv) # set the computed inverse to the internal cache
  
  inv # return the computed inverse
}

## How to run the two functions:
# > x = rbind(c(1, 4), c(4, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    4
# [2,]    4    1
# > cacheSolve(m)
# [,1]        [,2]
# [1,] -0.06666667  0.26666667
# [2,]  0.26666667 -0.06666667
# > cacheSolve(m)
# Inverse was cached. Getting cached inverse.
# [,1]        [,2]
# [1,] -0.06666667  0.26666667
# [2,]  0.26666667 -0.06666667
# > x = rbind(c(1,3), c(-3,-1))
# > m$set(x)
# > cacheSolve(m)
# [,1]   [,2]
# [1,] -0.125 -0.375
# [2,]  0.375  0.125
# > cacheSolve(m)
# Inverse was cached. Getting cached inverse.
# [,1]   [,2]
# [1,] -0.125 -0.375
# [2,]  0.375  0.125

