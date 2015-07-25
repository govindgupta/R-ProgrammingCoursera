# This function creates a matrix and it essentially contains 4 functions, they are :-
# 1. set() this function is used to set/reset the value of matrix. eg. a$set(matrix(10:7,2,2))
# 2. get() this function is used to get the stored value of the matrix. eg. a$get()
# 3. setInverse() this function can be used to manually set the inverse of the matrix. eg. a$setInverse(matrix(c(1,1,1,2),2,2))
# 4. getInverse() this function can be used to get the inverse of the matrix that is already stored. eg. a$getInverse()

# To use makeCacheMatrix "a <- makeCacheMatrix(matrix(1:4,2,2))" can be used without the quotes.

makeCacheMatrix <- function(x = matrix()) { # Creates a special matrix object that can cache its inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # return the matrix
  setInverse <- function(inverse) m <<- inverse # set the inverse of the matrix
  getInverse <- function() m
  list(set = set, get = get, # a list of functions is returned
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function is used to calculate the inverse of the matrix and if the inverse has already been calculated it is returned from cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse() # Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {   # If inverse is already set return the inverse
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) # Calculate the inverse
  x$setInverse(m)
  m # Return the matrix
}