## Put comments here that give an overall description of what your
## functions do

##  the function makeCacheMatrix creates a special "matrix", which is really a 
##    list containing a function to :
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverseMatrix) inv <<- inverseMatrix
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function calculates 
## the inverse of the special "matrix" created with the above function
## it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getInv();
        if(!is.null(inv)) {
              message("getting cached inverse")
              return(inv)
        }
    
        data <- x$get();
        inv <- solve(data, ...);
        x$setInv(inv);
        inv
}

## exemple of using the functions :
## diag1=diag(1:10);
## strucDiag1=makeCacheMatrix(diag1);
## invDiag1=cacheSolve(strucDiag1)



