## Cache the Inverse of a Matrix 
## Assumes an invertible matrix is input

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv<-NULL
  set<-function(y) {
    x<<-y
    mat_inv<<-NULL
  }
  get<-function() x
  setinv<-function(inv) mat_inv<<-inv
  getinv<-function() mat_inv
  list(set = set,get = get,setinv = setinv,getinv = getinv)

}


## Compute inverse returned by function  above, retrieves 
## inverse from cache if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinv(mat_inv)
  mat_inv
}
