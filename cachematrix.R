## Caching the Inverse of a Matrix
## Includes 2 functions: 
##    1.  makeCacheMatrix - creates a special "matrix" object 
##        that can cache its inverse
##    2.  cacheSolve - computes the inverse 
##        of the special "matrix" returned by makeCacheMatrix 
##        If the inverse has already been calculated 
##        (and the matrix has not changed), then the cachesolve 
##        should retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(inverse) invm <<- inverse
  getinvm <- function() invm
  list(set = set, get = get, 
       setinvm = setinvm,
       getinvm = getinvm)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message('getting cached data')
    return(invm)
  }
  data <- x$get()
  invm <- solve(data)
  x$setinvm(invm)
  invm
  
}

# #Sample run:
# x = matrix(rnorm(3*3), 3,3)
# cm = makeCacheMatrix(x)
# m$get()
# 
# 
# # The first run
# cacheSolve(cm)
# 
# 
# # The second run - getting cache data
# cacheSolve(m)
