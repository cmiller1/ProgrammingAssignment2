## Programming Assignment 2: Lexical Scoping
## Chris Miller, December, 2014

## The file contains two functions; the first takes a matrix
## as input and outputs a special object with four functions
## and the ability to cache its inverse

## The second function accepts one of these special matrix
## objects and computes the inverse. If the inverse has
## been previously calculated it skips the calculation. The
## function returns the inverse matrix.

## Variables Summary:
## start_matrix is the user defined input matrix
## m is used as a generic variable to store the inverse matrix and to test if it exists
## cachedmatrix is the special object created by the first function
## minv is the inverse matrix
## data is a temporary storage variable 

## This first function creates the matrix object with four functions that can be called on it
makeCacheMatrix <- function(start_matrix = matrix()) {
  m <- NULL
  ##the first function set allows a user to override the value of the input matrix
  ##Note: if a user changes the matrix with this function, the inverse will be re-calculated
  set <- function(y){
    start_matrix <<- y
    m <<- NULL
  }
  ##the function 'get' returns the matrix
  get <- function() start_matrix
  ##the function 'setinv' sets the global variable 'm' to equal the inverse
  ##note: there is a vulnerability here in that someone could manually set the inverse by calling this function, thus overriding the actual calculated inverse
  setinv <- function(minv) m <<-minv
  ##the function 'getinv' returns the current inverse (by default, NULL)
  getinv <- function() m
  ##the overall function returns a list of the function outputs
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##The second function checks to see if the inverse exists. If so, it returns this inverse. If not, it calculates it and calls the first function to store it as a global variable
cacheSolve <- function(cachedmatrix, ...) {
  m <- cachedmatrix$getinv()
  ##if the value already exists, just return that value
  if(!is.null(m)){
    message("getting cached value")
    return(m)
  }
  ##otherwise calculate the inverse and return that
  data <- cachedmatrix$get()
  m <- solve(data)
  cachedmatrix$setinv(m)
  ## Returns an inverse matrix
  m
}