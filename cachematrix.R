## Coursera Programming Assignment Week2:
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions are used to cache the inverse of a matrix.
## Check the end of the file to see the test run sequence.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  #initialized to null.
  #step: initialize
  myMatrix <- NULL
  
  
  # step1: set the value of the matrix
  set <- function(y) {
    x <<- y
    myMatrix <<- NULL
  }
  
  #step2: get value of the matrix
  get <- function() x
  
  #step3: set the inverse matrix
  setMatrix <- function(inverse) myMatrix <<- inverse
  
  #step4: get the inverse matrix
  getInverse <- function() myMatrix
  
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
  

}


## Returns a matrix that is inverse of 'x'"

cacheSolve <- function(x, ...) {
  
  myMatrix <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(myMatrix)) {
    message("getting cached data")
    
    # display matrix in console
    return(myMatrix)
  }
  
  data <- x$get()
  #we use solve to create the inverse matrix
  #no error handling needed, as we assume that the matrix is invertible
  myMatrix <- solve(data, ...)
  x$setMatrix(myMatrix)
  myMatrix
}



#Sample run. Use these steps to reproduce the results
#create a squared matrix: 
#> B = matrix(c(2, 4, 3, 1),nrow=2, ncol=2) 

#> m = makeCacheMatrix(B)

#> m$get()
# OUTPUT:
#[,1] [,2]
#[1,]    2    3
#[2,]    4    1

#first run without cache:
#> cacheSolve(m)
# OUTPUT:
#[,1] [,2]
#[1,] -0.1  0.3
#[2,]  0.4 -0.2


#second run with cache:
#> cacheSolve(m)
# OUTPUT:
# getting cached data.
# [,1] [,2]
# [1,] -0.1  0.3
# [2,]  0.4 -0.2