## cachematrix.R - R

## Caches an inputted matrix with auxillary information,
## namely it's inverse

## Caches a matrix and it's values

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(verse = matrix()) inv <<- verse
    getinv <- function() inv

    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Returns the inverse matrix of "x"

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message('thinking...')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
