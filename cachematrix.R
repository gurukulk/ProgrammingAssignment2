## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y){
    x <<- y  # assign the new matrix to x
    m <<- NULL  # reset the matrix value
    
  }
  get <- function() x    #return matrix which is saved
  setinv <- function(solve) m <<- solve   #using solve since it will compute matrix inverse
  getinv <- function() m  # returns / retrieves inverse matrix
  list(set=set,get= get, setinv=setinv, getinv=getinv)  #return the special matrix with defined functions

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data...")
    return(m)         # returns the cached value if it exists
  }
  data <- x$get()   # in case the cache doesn't exist then get the matrix
  m <- solve(data)  #compute the matrix inverse using solve R function
  x$setinv(m)       # Store the matrix inverse in cache
  m          #Return the matrix inverse
}
