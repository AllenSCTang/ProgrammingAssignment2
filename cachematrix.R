## This cachematrix() utilizes 2 sub-functions, makeCacheMatrix() and 
## cacheSolve(), to demonstrate the scoping rules of R by transmitting
## a matrix variable and a inverse matrix between them and the global
## working environment.

## Make a matrix ready to be calculated the inverse one. The calculation
## will be carried out and returned by cacheSolve(). While receiving 
## the answer, this function will put the answer into cache for future
## use by cacheSolve().

makeCacheMatrix <- function(x = matrix(),...) {
        invmatrix <<- NULL
        set <- function(new){
                x <<- new
                invmatrix <<- NULL
        }
        get <- function(){
                x
        }
        getinverse <- function(){
                invmatrix
        }
        setinverse <- function(inv){
                invmatrix <<- inv
        }
        list(set=set, get=get,
             getinverse=getinverse, setinverse=setinverse)
}


## Calculate the inverse of the matrix created in makeCacheMatrix().
## If the answer was calculated before and is still existed(cached), 
## this function will get and show the cached answer.
## If the answer isn't existed(cached), this function will calculate
## and show the answer, then return the answer to makeCacheMatrix()..

cacheSolve <- function(x, ...) {
        cal <- x$getinverse()
        if(!is.null(cal)){
                message("getting cached data")
                return(invmatrix)
        }
 
        ## Return a matrix that is the inverse of 'x'
        target <- solve(x$get(),...)
        x$setinverse(target)
        target
}
