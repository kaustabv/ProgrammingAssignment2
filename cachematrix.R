## This program helps to create a special square matrix, compute and cache 
## its inverse


## This function helps create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set_mat <- function(y) {                            # set matrix
                  x <<- y
                  inv <<- NULL
          }
          get_mat <- function() x                             # get matrix
          set_inv <- function(inv_matrix) inv <<- inv_matrix  # set matrix inverse
          get_inv <- function() inv                           # get matrix inverse
          list(set_mat = set_mat, get_mat = get_mat,
               set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special matrix created by makeCacheMatrix()
## If the inverse has already been computed, the following program simply retries 
## the inverse from the cache

cacheSolve <- function(x, ...) {
          inv <- x$get_inv()                                  # assign inv with value returned by get_inv()
          if(!is.null(inv)) {                                 # return cached value if inv is not a null matrix
                    message("getting cached data")
                    return(inv)
          }
          data <- x$get_mat()
          inv <- solve(data, ...)                             # compute inv 
          x$set_inv(inv)                                      # set inv
          inv
}