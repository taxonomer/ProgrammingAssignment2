## Programming Assignment 2 Answer

## Just like any other data operations, Matrix Inversion can be time consuming specially
## for large and complicated matrices. The below methodology shows how to cache the 
## solved inverse of a matrix and retrieve if for future use (such as when you do the 
## matrix inversion repeatedly without changing the given matrix).



## The makeCacheMatrix function creates a vector with 4 components (set the value of a matrix,
## get the value of a matrix, set the value of the inverted matrix, and get the value of the
## inverted matrix).

makeCacheMatrix <- function(x = matrix()) {
    inversem <- NULL
    
    set <- function(y){
        x <<- y
        inversem <<- NULL
    }
 
    get <- function(){
        x
    }   
    
    setInverse <- function(inverse){
        inversem <<- inverse
    }
    
    getInverse <- function(){
        inversem
    }
    
    list( set=set 
         ,get=get
         ,setInverse=setInverse
         ,getInverse=getInverse
         )
}



## The cacheSolve function returns the inverse of the matrix based from either a cached 
## solution for an existing matrix or a solution for a new matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inversem <- x$getInverse()
    
    if(!is.null(inversem)){
        message('getting cached data')
        return(inversem)
    }
    
    data <- x$get()
    inversem <- solve(data)
    x$setInverse(inversem)
}



