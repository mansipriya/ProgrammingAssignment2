## This program creates a special "matrix" object that can cache its inverse
## and computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has ##already been calculated (and the matrix ## has not changed), then the cachesolve should retrieve the inverse ##from the cache.


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL  
	#set the value of the Matrix
	setMatrix <- function(y) {
     		x <<- y
   		invMatrix <<- NULL
	}
 
	getMatrix <- function() x                              
	setInverse <- function(inverse) invMatrix <<- inverse  
	getInverse <- function() invMatrix                     
	list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse) 
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
	invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       
          message("Getting Cached Invertible Matrix")   
          return(invMatrix)                             
        }
          
	#if value of the invertible matrix is NULL then  
        MatrixData <- x$getMatrix()                      
        invMatrix <- solve(MatrixData, ...)             
        x$setInverse(invMatrix)                         
        return(invMatrix)                               	        
}
