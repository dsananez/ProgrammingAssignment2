## This functions calculates the inverse of a submited matrix. It then 
## caches that value to use it again over and over without making the
## calculation each time.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        set <- function(y) {
                x <<- y
                imat <<- NULL
        }
        get <- function() x
        setimat <- function(invmat) imat <<- invmat
        getimat <- function() imat
        list(set = set, get = get,
             setimat = setimat,
             getimat = getimat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                imat <- x$getimat()
                if(!is.null(imat)) {
                        message("getting cached data")
                        return(imat)
                }
                data <- x$get()
                imat <- solve(data)
                x$setimat(imat)
                imat
}
