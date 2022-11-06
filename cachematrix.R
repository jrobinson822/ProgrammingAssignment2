##makeCacheMatrix will store the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat<- NULL
  set<-function(y){
    x<<-y
  mat<<-NULL
}
get<-function() x
setinv<-function(InvMatrix) mat<<-InvMatrix
getinv<-function() mat
list(set=set,get=get,setinv=setinv, getinv=getinv)
}

##cacheSolve will use the matrix provided by makeCacheMatrix and calculate the inverse matrix 

cacheSolve <- function(x, ...) {
  mat<-x$getinv()  ##dollar sign symbol will access getinv
  if(!is.null(mat)) {
    return(mat)}
  df<-x$get()
  mat<-solve(df, ...)  ##mat finds inverse for the given matrix with the solve function
  x$setinv(mat)  
  mat  ##print the inverse of the given matrix
}
