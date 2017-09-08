## cachematrix.R - R

## Caches an inputted matrix with auxillary information,
## namely it's inverse

## Cache a matrix and it's values

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    ## Set a new value for the object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## Retrieve cached matrix
    get <- function() x

    ## Cache the inverse matrix
    setinv <- function(verse = matrix()) inv <<- verse

    ## Retrieve cached inverse matrix
    getinv <- function() inv

    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Return the inverse matrix of "x"

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()

    ## Return inverse matrix if already cached
    if(!is.null(inv)) {
        message('thinking...')
        return(inv)
    }

    ## Calculate inverse matrix and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
