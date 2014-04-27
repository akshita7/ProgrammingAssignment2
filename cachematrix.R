## cachematrix.R loads two functions, named, makeCacheMatrix(x = matrix()) and 
## chachSolve(x, ...), in the environment

## makeCacheMatrix(x) function creates a data structure to contain the given matrix,
## its inverse, and functions to retrieve the given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse 'i' as NULL
        i <- NULL
        
        ## set the given matrix and initial inverse value in the data structure  
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## gets the given matrix from the data structure
        get <- function() x
        
        ## sets the inverse matrix in the data structure
        setinverse <- function(inverse) i <<- inverse
        
        ## retrieves the inverse matrix
        getinverse <- function() i
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve(x, ...) function checks if the inverse matrix of the given matrix is present 
## in the environment and if found then returns it, otherwise it calcultes the inverse matrix,
## sets it in the data structure for future use and then returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## retrive the value of inverse matrix from the data structure
        i <- x$getinverse()
        
        ## check if the inverse is present and then return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## if inverse is NULL, then get the given matrix and solve for inverse
        data <- x$get()
        i <- solve(data, ...)
        
        ## set the calculated inverse in the data structure
        x$setinverse(i)
        
        ## return inverse matrix
        i
}
