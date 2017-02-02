# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) invers <<- inverse
  get_inverse <- function() invers
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  invers <- x$get_inverse()
  if(!is.null(invers)) {
    message("getting cached data.")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data)
  x$set_inverse(invers)
  invers
}

## Example:
#> x <- rbind(c(1,-2/3,3/2),c(4,5/6,-7/8),c(9,-4/3,-4/7))
#> m<- makeCacheMatrix(x)
#> m$get()
# [,1]       [,2]       [,3]
# [1,]    1 -0.6666667  1.5000000
# [2,]    4  0.8333333 -0.8750000
# [3,]    9 -1.3333333 -0.5714286
#> cacheSolve(m)
# [,1]      [,2]        [,3]
# [1,] 0.09570042 0.1386963  0.03883495
# [2,] 0.32558946 0.8196949 -0.40048544
# [3,] 0.74757282 0.2718447 -0.20388350
#> cacheSolve(m)
# getting cached data.
# [,1]      [,2]        [,3]
# [1,] 0.09570042 0.1386963  0.03883495
# [2,] 0.32558946 0.8196949 -0.40048544
# [3,] 0.74757282 0.2718447 -0.20388350
#> 