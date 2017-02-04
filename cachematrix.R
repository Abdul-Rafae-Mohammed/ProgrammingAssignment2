# The following two functions are used to cache the inverse of a matrix.

# This function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_val <- NULL
  set <- function(y) {
    x <<- y
    inverse_val <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inverse_val <<- inverse
  get_inverse <- function() inverse_val
  list(set=set, get=get, setinverse=set_inverse, getinverse=get_inverse)
}


# This function returns the inverse of the matrix.if the inverse has already been computed,
# it gets the result and skips the rest , else, it computes the invers and sets the value 
# in the cache using the set_inverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inverse_val <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached data.")
    return(inverse_val)
  }
  data <- x$get()
  inverse_val <- solve(data)
  x$setinverse(invers)
  inverse_val
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
