## Caching the inverse of a Matrix
## below functions are used to create a special object that will
## store a matrix and cache the inverse

## Special matrix that will cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)inv<<-inverse
  getInverse<-function()inv
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## This function returns the inverse from cache
## If inverse has been calculated and matrix has not changed
## it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
