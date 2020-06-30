## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# similar to makeVector()
# replaced m with inv and mean with inverse etc
# Example: my_matrix <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix(sample(1:16),4,4)) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
# similar to cachemean()
# replaced m with inv and mean() with solve() etc
# Example: cacheSolve(my_matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
