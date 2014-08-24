## The functions included below can be used to cache the inverse of a matrix


## makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse
    inv <- NULL
    
    ## setter method
    set <- function( matrix ) {
      mat <<- matrix
      inv <<- NULL
    }
    
    ## getter method
    get <- function() {
      ## return matrix
      mat
    }
    
    ## inverse setter method
    setInverse <- function(inverse) {
      inv <<- inverse
    }
    
    ## inverse getter method
    getInverse <- function() {
      ## return inverse 
      inv
    }
    
    ## retrun list with the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## cacheSolve will  the inverse from its cache if it has already been calculated
cacheSolve <- function(x, ...) {
    ## meturn inverse matrix of x
    mat <- x$getInverse()
    
    ## retrun inverse if already calculated
    if( !is.null(mat) ) {
      message("getting cached data")
      return(mat)
    }
    
    ## get the matrix from object
    matrix_data <- x$get()
    
    ## calculate the inverse
    mat <- solve(matrix_data) %*% matrix_data
    
    ## set inverse to object
    x$setInverse(mat)
    
    ## return matrix
    mat
}
