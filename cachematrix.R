## This functions calculate de inverse of a matrix, and stores the result as an object 
## in order to cache the calculated Inverse Matrix for a better performance
## Premises :
## 
## Doesn't validate the matrix is "invertible"


## Stores the "environment" associated with the object, the associated functions 
## ( setters  and getters ) and the values of variables from each environment. 

makeCacheMatrix <- function(X = matrix() ) {
    Inverse <- matrix()
    set <- function(y) {
        X <<- y
        Inverse <<- matrix()
        return(X)
    }
    get <- function() X
    setinverse <- function(Inversed) Inverse <<- Inversed
    getinverse <- function() Inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Validates if we have "solved" the input matrix, using the method "getinverse" and 
## evaluating if all the values of the matrix "Inverse" are not NA, that means we have
## calcuated the inverse matrix previously. 
  

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    Inverse <- X$getinverse()
    ## the next if is for validate if we have calculated the inverse matrix previously
    if(!all(is.na(Inverse) )){
        message("getting cached data")
        ## if we do... write a message and return the inverse matrix 
        return(Inverse)
    }
    ## if wet get here, the previous if is false, so we must calculate de inverse matrix
    ## to do this, we retrieve  the matrix:
    D <- X$get()
    ## use the function solve :
    Inverse <- solve(D)
    ## cache the result in the Object associated :
    X$setinverse(Inverse)
    ## return the result
    Inverse
}
