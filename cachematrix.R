# It will create a list containing a function to set the value of a matrix, get the value of the matrix, set the value of the
# inverse of said matrix and lastly, get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y)
                {
                x <<- y
                inv <<- NULL
                }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Now, this function will return of the inverse of the matrix. As the example, it first check if the inverse has already
# been calculated, in which case it gets the result without calculating. If not, it calculates the inverse.

cacheSolve <- function(x, ...) 
{
        inv <- x$getinverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
