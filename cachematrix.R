## Put comments here that give an overall description of what your
## functions do

## The following function creates a list object used to cache a real matrix and 
## its inverse. 
## The returned list contains functions to 
## set the matrix
## get the matrix
## set the matrix inverse
## get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y){
            x <<- y
            inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(invMat) inverseMatrix <<- invMat
        getinverse <- function() inverseMatrix
        ## returned list
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix stored in the 
## list object returned by the previous function. 
## It first checks if the inverse matrix has already been calculated (thus, if
## it already exists in the list object). If so, it use the `getinverse` method 
## to get it, otherwise it calculates it and set it in the list object using 
## `setinverse`

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if (!is.null(inverseMatrix)){
                message("getting chaced data")
                return (inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
