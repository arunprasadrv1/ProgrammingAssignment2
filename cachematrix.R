## The two functions below are constructor functions and inverse functions of matrix respectively


## The constructor function provides 4 functions to access a matrix vector 
## Overall the value of the inverse matrix is stored in cache with the data 
## AP
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                 #initialize inverse to NULL local scope
        getinverse <- function() i
        get <- function() x
        setinverse <- function (inv) i <<- inv
        set <- function(y) {x<<-y; i<<-NULL} #as data changes, inverse gets reset
        
        #return all functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function accepts square inversible matrix and returns inverse from cache if available
## If inverse unavailable in cache, then computes the inverse of matrix
## AP
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()     #retrieve inverse from global var
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # If inverse not available, compute inverse below
        data <- x$get()    
        i <- solve(data,...)    #compute inverse
        x$setinverse(i)         #store inverse in global var
        
        i                       #return inverse
}
