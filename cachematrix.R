## Create cached matrix data type  and cachesolve() function
##
##
## To test functions following commands can be used:

## exp.matrix <- matrix( rnorm(10000), nrow = 100, ncol = 100 )
## cache.exp.matrix <- makeCacheMatrix(exp.matrix)
## inv.matrix <- cacheSolve(cache.exp.matrix)
## sum(exp.matrix %*% inv.matrix) # should result 100  is there any smart way to test identity matrix?

## to cache test continue with
## inv.matrix <- cacheSolve(cache.exp.matrix)
## sum(exp.matrix %*% inv.matrix) # should also result 100  is there any smart way to test identity matrix?



makeCacheMatrix <- function(x = matrix()) {
  
  # to enable debug print outs assign true to DEBUG
  DEBUG <- FALSE
  
  if (DEBUG) {
    print( "DEBUG: makeCacheMatrix entrance")
  }
      
  # define and empty inv.matrix
  inv.matrix <- NULL
    
  # set()
  # 
  # changes defined matrix to new one and clear cache
  set <- function(y) {
  
      if (DEBUG) {
            print( "DEBUG: set()")
      }
        
      x <<- y
      inv.matrix <- NULL
        
  }
  
  # get()
  # 
  # returns matrix
  
  get <- function() {
    
    if (DEBUG) {
      print( "DEBUG: get()")
    }
    
    x
  }
  
  # setmatrix
  # 
  # returns matrix
  setinv <- function(inverse) {

    if (DEBUG) {
      print( "DEBUG: setmean()")
    }    
    
    inv.matrix <<- inverse
    
  }

  # setmatrix
  # 
  # returns matrix  
  getinv <- function() {
    
    if (DEBUG) {
      print( "DEBUG: getmean()")
    }  
    
    inv.matrix
  }
  

  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}



##
##
## This function use solve() function to get matrix inverse
## If inverse matrix calculated before it returns cached dat
##
cacheSolve <- function(x, ...) {

  
  # to enable debug print outs assign true to DEBUG
  DEBUG <- FALSE
  
  if (DEBUG) {
    print( "DEBUG: cacheSolve entrance")
  }
  
  inv.matrix <- x$getinv()
  # if inverse matrix calcualted before:
  if (!is.null(inv.matrix)){
    message("getting cached data")
    inv.matrix
  } else {
  
  #  new matrix, new calculation:
  data <- x$get()           #get matrix
  
  inv.matrix <- solve(data) #calculate inverse
  
  x$setinv(inv.matrix)      #set cache
  inv.matrix
  }
  
}


