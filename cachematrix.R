## These two functions work together to cache and retreive cached inverses of matrices

## This function is to return a list of functions that can be used to store a matrix
## and the cached inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  #set up cache to NULL 
  cache<- NULL
  
  #Set the Matrix based on incoming parameter
  setMatrix<- function (y){
    x<<- y
    cache<<- NULL
  }
  
  #Get the matrix currently storied
  getMatrix<- function(){
    x
  }
  
  #Set the cache to the new inversed matrix
  setInverse<- function(cached){
    cache<<-cached
  }
  
  
  #Return the cached inverse matrix
  getInverse<- function(){
    cache
  }
  
  
  #Return the list of functions
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
  
}


## Calculate the inverse of the matrix as defined in makeCacheMatrix
## Then reuse the cached result if it exists. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed<- x$getInverse()
        if(!is.null(inversed)){
          message("getting cached data")
          return(inversed)
        }
        
        ##if we don't have the inverse already cached, then we'll have to create it and 
        ##cache it for the future
        newInverse<- x$getMatrix()
        inversed<- solve(newInverse)
        x$setInverse(inversed)
        
        #Return inversed matrix
        inversed
}
