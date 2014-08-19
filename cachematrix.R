## This functions calculates the inverse of a submited matrix. It then 
## caches that value to use it again over and over without making the
## calculation each time.

## makeCacheMatrix will create a list of functions that will
## be called by cacheSolve. Calling makeCacheMatrix wont call
## any of this 3 functions.

makeCacheMatrix <- function(x = matrix()) {  ## Inpunt = Matrix.
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


## cacheSolve accesses the object ‘x’ (Matrix) and gets the inverse using solve().
## If inverse was already cached it sends “getting cached data" to the console and 
## returns the inverse matrix ennding the function. However, if the inverse isn’t cached, 
## then it calculates it and store it.

cacheSolve <- function(x, ...) {  ##Input = Object created with makeCacjeMatrix
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
