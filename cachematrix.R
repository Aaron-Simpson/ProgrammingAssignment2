#These functions are supposed to save computation time when dealing with matrix inverses 
#by caching the inverse of a matrix rather than calculating repeatedly.

#makeCacheMatrix takes as its input an invertible matrix and creates a cache to store its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  Inv<-NULL
  set<-function(y){
    x<<-y
    Inv<<-NULL
  }
  get<-function() x
  setInv<-function(solve) Inv<<-solve
  getInv<-function() Inv
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


#cacheSolve takes the value of makeCacheMatrix for a particular matrix and returns the cached inverse, 
#if it exists, and caculates then returns the inverse if there is no cashed inverse.
cacheSolve <- function(x, ...) {
  Inv<-x$getInv()
  if(!is.null(Inv)){
    message("getting cashed data")
    return(Inv)
    }
    else
    Dodo<-x$get()
    Inv<-solve(Dodo, ...)
    x$setInv(Inv)
    Inv
}
