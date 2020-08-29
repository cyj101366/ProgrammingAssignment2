## Put comments here that give an overall description of what your
## functions do

## creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    } ## set the value of matrix
    get <- function()
    {
        x
    }## bring the data, x
    setinverse <- function(inverse)
    {
        inv <<- inverse
    }## set inv as x's inverse matrix
    getinverse <- function()
    {
        inv
    }## bring the data, inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## read the x, calculate inv_x, return inv_x
cacheSolve <- function(x, ...)
{
    
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    ## gets data from x
    data <- x$get()
    
    ## Calculating inverse from data -> save to inv
    inv <- solve(data, ...)
    ## End of inverse calculation
    
    x$setinverse(inv)
    inv
    ## Save & return a matrix which is the inverse of 'x'
}
