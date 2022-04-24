## The following two main functons save the input of matrix as caches
## and calculate the inverse of the cached matrix

## This function creates an object X to store a matrix, and is a really
## a list containing a functon to
##      1. Set the value of the matrix
##      2. Get the value of the matrix
##      3. Set the value of the inverse
##      4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<- function()x
        setinverse<-function(inverse)m<<-inverse
        getinverse<-function()m
        list(set=set, 
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## This function transfer the returned matrix from makeCacheMatrix into
## its inverse. If the inverse has already been cached and the matrix
## has not changed, then the cacheSolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
