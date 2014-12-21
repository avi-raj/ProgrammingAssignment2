## makeCacheMatrix() creates an object to cache the inverse of an input matrix 
## CacheSolve() references this object and returns its inverse.
##     If the matrix inverse was calculated already (the result is cached),
##     the cached value is returned
## Code has been extensively commented to explain every step of the process
## Usage examples are given at the end of this file

## makeCacheMatrix creates an object and stores the input matrix and it's inverse
## the inverse matrix is cached & is initially set to NULL
makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix
  
  m <- NULL    #  m will be our 'inverse' and it's reset to NULL every 
               #  time makeCacheMatrix is called
  
  #   THe next three functions are defined but not run when makeCacheMatrix is called.
  #   instead, they will be used by cacheSolve() to get values for x or for
  #   m (solve) and for setting the inverse.  These are the object 'methods'
  
  get <- function() { x }   # this function returns the value of the original matrix
  
  setsolve <- function(solve)  { m <<- solve }
                            #  Called by cacheSolve() during the first cacheSolve()
                            #  access and it will store the value using superassignment
  
  getsolve <- function() { m } # this will return the cached value to cachesolve() on
                               # subsequent accesses
  
  list(get = get,            #  Accessed each time makeCacheMatrix() is called,       
       setsolve = setsolve,  #  that is, each time we make a new object.  This is a list of 
       getsolve = getsolve)  #  the internal functions ('methods') so a calling function
                             #  knows how to access those methods.                            
}

## Returns inverse of a matrix, if previously cached it returns the cached value 
cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  m <- x$getsolve()                # accesses the object 'x' and gets the matrix inverse
  if(!is.null(m)) {                # if the matrix inverse was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the cached matrix inverse ... "return" ends 
                                    #   the function cacheSolve()
  }
  data <- x$get()         # we reach this code only if x$getsolve() returned NULL
  m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse of the matrix
  x$setsolve(m)           # store the calculated  value in x (see setsolve() in makeCacheMatrix
  m                       # return the matrix inverse to the code that called this function
}

## Usage : Please follow Steps 1,2,3 below 
## Step1 : Pass a sample matrix to makeCacheMatrix
## x<-matrix(c(4,3,3,2),2,2)
## mymatrix<-makeCacheMatrix(x)
## OR
## mymatrix<-makeCacheMatrix(matrix(c(4,3,3,2),2,2)) 
## Step2 : calculate the inverse
## cacheSolve(mymatrix) 
## Step3 : cacheSolve(mymatrix) again, to see cached value is returned. (a message is included)
## cacheSolve(mymatrix)