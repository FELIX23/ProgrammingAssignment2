
##This function creates a special "matrix" object that can cache its inverse

## Use Example

## a = matrix(c(2,3,4,5), nrow=2, ncol=2)
## a$get  # Get the matrix
## cacheSolve(a)  # Get the inverse of the matrix, the first time the operation is don

## If we do cacheSolve(a) any other time, the operation is not done anymore, unless 
## the matrix is changed. The value is taken from the chache.
## cacheSolve(a)

## Must return : getting cached data, prior to the inverse matrix.


makeCacheMatrix <- function(x = numeric()) {
    
   ## Loading the cache with NULL value
   cache <- NULL
  
   ## Function to set a value to the matrix and updating cache
    set <- function(matrix) {
      x <<- matrix
      cache <<- NULL
    }
    
   ## Function to get the value 
    get <- function() x
    
    ##  Cache argument
    setinverse <- function(solve) cache <<- solve
    ## Get cached value
    getinverse <- function() cache
    ## List return. Each element is given by each prior function.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
  

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x) {
    ## get inverse value, cache.      
    m <- x$getinverse()
    
    ## Check if the value is chaced. In that case, no calculation is done.
    if(!is.null(m)) {
      message("getting cached data")
      ## Returning cached value
      return(m)
    }
    ## In the case the valued is not cached, calculation is done with the function 
    ## solve
    
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
  }  
  





