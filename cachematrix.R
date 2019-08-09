## Function with cache the inverse of a matrix

## Creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y){
                x<<- y
                inv<<- NULL
        }
        get<- function()x
        setinverse <-function(inverse) inv<<- inverse
        getinverse <-function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


##Computes the inverse of matrix created by makeCacheMatrix


        ## Return a matrix that is the inverse of 'x'

        cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
                
        }

        
