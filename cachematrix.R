
## A function that received a special "matrix" object and cache the inverse
## of the matrix and store in the global environment


makeCacheMatrix <- function(x = matrix()) {
  ##initialize the inverse matrix  
  inv <- NULL
  #set function for the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##define the return matrix object by defining it as get
  get <- function() {
    x
  }
  ##setting the inverse matrix 
  setInverse <- function(inverse) {
    
    ##assign the inverse matrix to the environment var
     inv <<- inverse
  }
    
  ##define the function for returning the inverse matrix  
  getInverse <- function() {
    inv
    
  }
  ##put all defined function into a list for future call within the environment
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      
  inverse <- x$getInverse()
  
  if (!is.null(inverse)){
    if ( identical(x$get() %*% inverse, inverse %*% x$get())) {
      print ("Getting cached data")
      return(inverse)
    }
    
    
  }
  
  #Else inverse matrix null or matrix change, calculates the inverse 
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  
  inverse
    
}
