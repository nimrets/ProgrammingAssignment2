

##  Function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #function for the matrix to store
      inverse <- NULL ## here will store future inverse
      set <- function (y){ ## need to update the matrix stored and reset the cached inverse 
            x <<- y
            inverse <-- NULL
      }
      get <- function() x  ## return the stored matrix
      setInv <- function(inv) inverse <<- inv ## assign the value of inverse
      getInv <- function () inverse ## get the value when calling inverse.
      list (set=set, get=get, setInv=setInv,getInv=getInv)
 }


##  Inverse of the special “matrix” returned 

cacheSolve <- function(x, ...) { 
      inverse <- x$getInv()   ##Get the cached inverse
      if(!is.null(inverse)){   ## If the inverse is not NULL we get the data
            message("Getting cached data!")
            return(inverse)
      }
      mat <- x$get() ## get the original matrix
      inverse <- solve(mat,...)  #compute the inverse
      x$setInv(inverse)
      inverse
}
