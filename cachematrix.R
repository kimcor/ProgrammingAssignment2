# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

>makeCacheMatrix <- function(x = matrix()) { 
  Inv <- NULL ## begins by setting the inv to null as a placeholder for a future value
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## returns the vector, x
  setinverse <- function(inverse) inv <<- inverse ##sets the inverse, inv
  getinverse <- function()inv ## returns the inverse, inv
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## returns the 'special vector'containing all of the functions just defined
}

## CacheSolve function is supposed to calculate the inverse 
## Return a matrix that is the inverse of 'x'

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- functon(x,...){
  inv <- x$getinverse() ##assigns inv in cachesolve, the value from getinverse(inv) that is NULL
  if(!is.null(inv)){ ##if the inverse stored under the parameters 'vector x'is not NULL, return it
    message ("getting cached data.")
    return (inv) ## function will stop the function from further execution and immediately return the cache inv value
  }
  data <- x$get() ##assign the data to vector x
  inv <- solve(data) ##calculate the mean and assign it to inv
  x$setinverse(inv) ##store the inverse 'inv'under the parameters "vector x"
  inv
}
