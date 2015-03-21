

## makecachematrix function creates a special "matrix", which contains a function to
## set the matrix, get the matrix, set the inverse matrix, get the inverse matrix


makeCacheMatrix<-function(x = matrix()) {
    m <-NULL
    set <-function(y) {
        x <<-y
        m <<- NULL
    }
     get<- function() x
    setinverse <-function(solve) m <<-solve
    getinverse <-function() m
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The cacheSolve function calculates the inverse matrix of the special matrix created with the makeCacheMatrix function.
## If the inverse matrix has already been calculated it gets the inverse matrix from the cache and skips the computation.
## If it's not it calculates the inverse matrix and sets it in the cache.
    
cacheSolve <- function(x=matrix(), ...) {
    m <-x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
    matrix <-x$get()
    m <-solve(matrix, ...)
    x$setinverse(m)
    m
}



