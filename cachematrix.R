## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#The first function, makeCacheMatrix is exactly like amkeVector
# it creates an inverse "matrix" by using following functions:
#set the value of the matrix
#get the value of the matrix
#set the value of the inveresed matrix
#get the value of the inveresed matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#The following function is exactly the function provided in the example
# it produces the inverese of the matrix created with the above function. 
#However, it first checks to see  if the inverese matrix has already been produced 
#If so, it gets the inveresed matrix from the cache 
#Otherwise, it produces the inverese of the matrix 
#then it changes the value of the inveresed matrix in the cache 

cacheSolve <- function(x, ...) {
  inv_mat <- x$getInverse()
  if (!is.null(inv_mat)) {
    message("Getting the Cached Inveresed Matrix")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$setInverse(inv_mat)
  inv_mat
}
