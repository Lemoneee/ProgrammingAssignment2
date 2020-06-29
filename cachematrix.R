## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Write a short comment describing this function
## This function creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
  x<<-y
inv<<-NULL
   }
        get<-function()x
        setinverse<-function(solvematrix)inv<<-solvematrix
        getinverse<-function()inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.
## If the inverse has already been calculated, then the
## inverse can be returned from the cache for the cacheSolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data)
        x$setinverse(inv)
        inv
}
