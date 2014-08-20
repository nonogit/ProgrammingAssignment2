############################################################## 
## The function makeCacheMatrix creates a list containing 4 functions:
## 1- "set" is used to declare a new data container.
## 2- "get" is used to retrieve the data in the container.
## 3- "setinverse" is used to save the value of the inverse of the matrix 
##     in "cache"
## 4- "getinverse" is used to retrieve the saved value of the inverse of          
##     the matrix in "cache"
###############################################################

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function(){x}
        setinverse <- function(inverse) {m <<- inverse}
        getinverse <- function() {m}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


############################################################## 
## The function cacheSolve evaluates whether the inverse of the matrix is 
## already calculated and stored in "cache". If so, it prints a 
## message confirming, returns its value, and finishes. If not, it calculates 
## the inverse and stores it in "cache" for future use.
###############################################################

cacheSolve <- function(x, ...) {
        m <- x$getinverse()             # try to fetch value from cache
        if(!is.null(m)) {               # if the value exists...
                message("getting cached data")
                return(m)
        }
        data <- x$get()                 # if the value does not exist...
        m <- solve(data)
        x$setinverse(m)
        m
}
