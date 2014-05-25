
## Write a short comment describing this function - makeCacheMatrix
## The function takes a matrix argument and creates a list containing the four functions required to resolve cacheSolve 


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setMatrix<-function(solve) m<<- solve
  getMatrix<-function() m
  list(set=set, get=get,
       setMatrix=setMatrix,
       getMatrix=getMatrix)
}

## Write a short comment describing this function - cacheSolve
## Takes List produced using makeCacheMatrix and outputs inverse matrix for the original vector
##x is passed to cachSolve. cachSolve checks to see if m has has been caculated for x, 
##if m is not NULL return "message" and m
## othewise calculate/solve x and setMatrix assigns value to m for the cache,
## return m
cacheSolve <- function(x, ...) {
  m<-x$getMatrix()
  if(!is.null(m)){                 
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setMatrix(m)
  m
}
