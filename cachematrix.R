## These functions will a special matrix object that can cache its inverse .  This can be useful since inverting matrices (especially large 
## matrices) can be time consuming and resource intensive.

## makeCacheMatrix will perform 4 roles
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y)  {
                x <<-y
                inv <<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)        
}


## cacheSolve will compute the inverse of the  the matrix if necessary.  
## If the inverse is already computed, it gets the result and skips the computation.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        a <- x$getinverse()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        print(a)
