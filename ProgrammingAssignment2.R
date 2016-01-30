

## These two functions create a matrix and its inverse from cache
makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix creates a custom matrix 
      ## set helps to store the matrix in cache
      ## setInverse and getInverse stores the inverse matrix in cache
      
      makeCacheMatrix <- function(x = matrix()){    
          m <- NULL
          set <- function(y){
              x <<- y  
              m <<- NULL # Matrix in cache 
            }
          mat <- function() x # Get matrix
          setInverse <- function(solve) m<<- solve # Inverse matrix
          getInverse <- function() m # Get inverse matrix
          list(set = set, mat = mat,
                       setInverse = setInverse,
                       getInverse = getInverse)  ## List of functions
      }
      
      
        ## cacheSolve takes the custom matrix created by makeCacheMatrix function and does its inverse
        ## If the calculation has already been done it takes data from cache otherwise it stores it in cache
    
    
        
        cacheSolve <- function(x, ...) {
                 ## Inverse of matrix x
               m <- x$getInverse()                 
               if(!is.null(m)){                    # if it is cache the inverse has already been calculated
                   message("cache data")     
                  return(m)                           
                 }
               data <- x$get()                     # get the matrix used by makeCacheMatrix function 
               m <- solve(data, ...)               # calculate the inverse of the matrix
               x$setInverse(m)                     # store the inverse matrix in cache using the makeCacheMatrix set function
        }
        
        
