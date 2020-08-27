## To boost the efficiency of matrix inversion, it is better to cache the inverse of a matrix rather than compute it repeatedly. 
## So this function aims to cache the inverse of a matrix.

## Using "makeCacheMatrix" function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set<-function(y){
        x<<-y
        s<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse)s<<-inverse
    getinverse<-function()s
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Using "cacheSolve" function to compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## The first thing to do with this function is to check to see if the inverse has already been calculated.If so, the cachesolve should retrieve the inverse from the cache.
## Otherwise, it computes the inverse of the special "matrix".

cacheSolve <- function(x, ...) {
    s<-x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        reture(s)
    }
    data <- x$get()
    s<-solve(data,...)
    x$setinverse(s)
    s
}
    

