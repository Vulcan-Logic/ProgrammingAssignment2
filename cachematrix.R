## makeCacheMatrix - store the matrix and it's inverse in cache memory to speed up operation
## cacheSolve - a small toy example to demostrate how to use chached objects

## store the matrix and it's inverse in chache memory to speed up operations
makeCacheMatrix <- function(x = matrix()) {
    set <- function(y) {     #set/store the matrix values
      if (!is.null(x)){      #check if supplied matrix is not null
        x <<- y              #store the matrix value in cache
        mInverse <- NULL     #set the inverse to NULL
      }
      else if (x!=y){       #check if matrix has changed
        message("stored matrix has been changed")
        x <<- y             #store the matrix value in cache
        mInverse <- NULL    #set the inverse to NULL
      }
    }
    get <- function() x                                      #return the stored matrix
    setInverse <- function(inverse) mInverse <<- inverse     #store the matrix inverse in cache
    getInverse <- function() mInverse                        #return the matrix inverse
    return(list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse))
}




## cacheSolve - a small toy example to demostrate how to use chached objects. check to see if the inverse of a matrix is  
## Stored in cache if so, extract and return it, otherwise calculate the inverse and store it back in cache.  
cacheSolve <- function(x=NULL, ...) { 
  inverse<-NULL               #set return value to NULL 
  if (!is.null(x)){           #check if a valid function list has been passed
    inverse <- x$getInverse() # get the inverse from cache
    if(!is.null(inverse)) {   #check to see if inverse is not NULL
      message("returning stored inverse of stored matrix")
      return(inverse)
    }
    else {
      message("stored inverse not found, computing inverse and storing it in cache")
      data <- x$get()         #get the stored matrix from cache
      inverse <- solve(data)  #calculate the inverse
      x$setInverse(inverse)   #store the inverse
    }
  }
  return(inverse) 
}
