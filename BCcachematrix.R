## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function returns a list of fucntions that will be called in the cachSolveFun
makeCacheMatrix<-function(x = matrix()){ #the input object to this fun is the matrix to be inverted
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInvMat <- function(InvMat) im <<- InvMat #assign the value to the m variable
    getInvMat <- function() im
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}  


cachSolve <- function(x, ...) { # the input object to this fun is the makeCahematrix object
    im <- x$getInvMat() #check whether the Inverse matrix has been already computed
      #if so return its value, otherwise compute it.                 
    if(!is.null(im)) {
        message("getting cached Inverse Matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInvMat(im)
    im
}
