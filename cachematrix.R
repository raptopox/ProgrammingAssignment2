## A pair of functions that help maintain a structure for caching a matrix and its inverse (makeCacheMatrix(x)), 
## ...and computing and caching its inverse (cacheSolve())

## makeCacheMatrix(x) prepares and handles a matrix structure able to chache its inverse. 
## The output of the function is a list of 4 functions able to set and get the matrix value, and set and get its inverse  
## The input of the function is a matrix x, but it is really optional and we can set the value of the matrix using the set function. 
## ...If we use do not use the set function, the value of x is not chached.  

makeCacheMatrix <- function(x = matrix()) { # because of this initialization of x, we can find the inverse without first calling set()
  # Variables of makeCacheMatrix(x): x (formal), inverse (local)
    
  # Initialization: Inverse is a local variable for makeCacheMatrx() that will contain the inverse.
  inverse<-NULL 
  
  # function makeCacheMatrix$set(y), which caches matrix y to x and "resets" its inverse
  set<-function(y){ 
    # Variables of makeCacheMatrix$set(y): y (formal), x (free), inverse (free)
    
    # y is the matrix that we want to cache 
    x<<-y  
    inverse<<-NULL 
  }
  
  # function makeCacheMatrix$get(), just "shows" x
  get <- function() x
  # Variables of makeCacheMatrix$get(): x (free)
    
  # function makeVacheMatrix$setinverse(inv) caches the value of inv as the inverse of the matrix x.
  setinverse <- function(inv) inverse <<- inv  
  # Variables of makeCacheMatrix$setinverse(inv): inv (formal), inverse (free)  
  
  # function makeCacheMatrix$getinverse(), just "shows" the inverse of x
  getinverse <- function() inverse
  # Variables of makeCacheMatrix$getinverse(): inverse (free)
  
  # the final output of function makeCacheMatrix(x) is a list containing the 4 above functions that "handle" x and its inverse  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve can be used to compute the inverse of the matrix contained in the special matrix structure x 
## The input of the function is a special matrix structure made by makeCacheMatrix(x), hence x here is not just a matrix
## ...(see also comment on cacheSolve variables). 
## The output of the function is the inverse of the matrix. If nothing has changed, then the cached inverse is returned.

cacheSolve <- function(x, ...) {
  # Variables of cacheSolve(x,...): 
  # x (formal; this is not the same x formal variable used in makeCacheMatrix, due to lexical scoping; x here is a list of functions), 
  # inverse (local; this is not the same inverse local variable used in makeCacheMatrix, due to lexical scoping), 
  # data (local)
  
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  
  # if there are no new data, return cached versions and exit
  if (!is.null(inverse)){
    message("getting cached inverse")
    return(inverse) 
  }
  
  # ...otherwise, get new data
  data<- x$get() 
  
  # ...and get the inverse; "..." stands for other arguments passed from other methods
  inverse <- solve(data, ...)
  
  # ...and cache the inverse to the matrix structure 
  x$setinverse(inverse)
  
  # ...and finally return the inverse
  inverse
  
}
