## To solve the inverse of one matrix, we apply solve() function on
## matrix data. Because this process consumes a lot of system time, 
## we would like to directly retrieve the cached inverse matrix if 
## this one has been solved before to save time. If the matrix has 
## not been encountered before, we solve it using solve() function.

## As a function generating a list of functions, this part get the 
## matrix whose inverse needs to be solved, and set and get the 
## resulted inverse into memory(m object here). 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverMatrix <- function(InverMatrix) m <<- InverMatrix
    getInverMatrix <- function() m
    list(set = set, get = get,
         setInverMatrix = setInverMatrix,
         getInverMatrix = getInverMatrix)
}


## This part check if the memory object(m) has saved the inverse of 
## this matrix: if it has, directly return the cached one; if it   
## has not, use solve() function to return the inverse and set the 
## result as m using setInverMatrix() function which has been 
## created in the first part. 

cacheSolve <- function(x, ...) {
    m <- x$getInverMatrix()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setInverMatrix(m)
    m
}
