## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation, 
## and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## Write a short comment describing this function

## makeCacheMatrix takes an argument x of type matrix vector. It 
## returns a list of four items (which are functions). They set the
## value of the matrix, get the value of the matrix, set the value 
## of the inverse matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function

## cacheSolve will query the x vector's cache. If there is a cache, 
## it returns the cache; no computation needed. If there is no cache, 
## it computes them. Then it saves the result back to x's cache, 
## and returns the result.

cacheSolve <- function(x, ...) {
  
  
    m <- x$getinverse()      #query the x vector's cache 
    if(!is.null(m)) {        #if there is a cache
      message("getting cached data")
      return(m)              #just return the cache; no computation needed
    }
    data <- x$get()          #if there is no cache
    m <- solve(data, ...)    #compute them here
    x$setinverse(m)          #save the result back to x's cache
    m                        #return the result
  
        ## Return a matrix that is the inverse of 'x'
}
