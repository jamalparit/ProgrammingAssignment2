## This is my Second Assignmet is to write two functions 
## "makeCacheMatrix" and "cacheSolve" 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

  }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##m <- matrix(rnorm(16),4,4)
##> m1 <- makeCacheMatrix(m)
##  -------------------------------------------------------------------------------
##Error in makeCacheMatrix(m) : could not find function "makeCacheMatrix"
##> source('D:/OneDrive/Public/Data Science/ProgrammingAssignment2/cachematrix.R')
## --------- Error Forgot To Run File cachematrix.R  ------------------------------
## --------------------------------------------------------------------------------
##  > m1 <- makeCacheMatrix(m)
##> cacheSolve(m1)
##[,1]        [,2]       [,3]       [,4]
##[1,]  0.567826930  0.03801842  0.1050177 -2.6377331
##[2,]  0.255597945  0.23305894 -0.1402727  0.7669624
##[3,]  0.530958036 -1.52856201  0.5124520 -2.0886566
##[4,] -0.005010119  0.37752277 -0.7705128  1.3580065

