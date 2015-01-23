## Put comments here that give an overall description of what your
## functions do

## This function caches matrix and inverse and provides access to set and get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

 ## Initialize matrix and its inverse
  mat <- matrix(,0,0)
  invmat <- NULL
  
  ## this is a subfunction to set the matrix
  setmatrix <- function(mtx) {
    ismatrixsame <- F
    
    ## Check to see if the supplied matrix is same as already cached matrix
    if((nrow(mat) == nrow(mtx)) & (ncol(mat) == ncol(mtx))){
          comparematrix <- mat == mtx
          if(length(mtx[!comparematrix]) == 0){
            ismatrixsame <- T
            print("Supplied matrix is the same as cached matrix")
          }    
          
      
    }
    
    ## Store the matrix only if it is different from the cached matrix
    if(ismatrixsame==F){
      print("Caching the supplied matrix")
      mat <<- mtx
      invmat <<- NULL
    }
    
  }
  ## This is a subfunction to return the cached matrix
  getmatrix <- function() mat
  
  ## This subfunction caches supplied inverse matrix
  setinversematrix <- function(inverse) invmat <<- inverse
  
  ## This is a subfunction to return the cached inverse matrix
  getinversematrix <- function() invmat
  
  ## Below line calls the setmartix function with supplied matrix when "makeCacheMatrix" is first called
  setmatrix(x)
  
  ## Return a list with cached matrix and its inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)


}


## This function calculates and stores the inverse of matrix. if inverse is already cached, it will get the cached inverse otherwise calculates and stored the matrix inverse
cacheSolve <- function(x, ...) {

 ## Get the inverse of the matrix from cache
  inversematrix <- x$getinversematrix()
  
  ## Check if the inverse matrix from cache is null and calculate inverse
  if(!is.null(inversematrix)) {
    message("getting cached data")
    
  }else {
    
    ## Get the cached matrix
    mat <- x$getmatrix()
    
    ##Calculate inverse of the cached matrix
    inversematrix = solve(mat)
    
    ## Cache the calculated inverse
    x$setinversematrix(inversematrix)
    
  }
  
  ## return the inverse matrix
  inversematrix


}
