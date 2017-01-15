## makeCacheMatrix: creates a matrix able to cache its inverse
## cacheSolve: retrieves inverse or computes it 

## Caching Inverse of Matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Retrieves inverse 
## Or, if numbers have changed, computes and stores. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!isnull(i)){
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
