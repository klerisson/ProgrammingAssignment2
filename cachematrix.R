## Put comments here that give an overall description of what your
## functions do

##Function to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        mt <- NULL
        
        set <- function(y){
                x <<- y
                mt <<- NULL
        }
        get <- function(){
                x      
        } 
        
        setInvertedMatrix <- function(solve) {
                mt <<- solve(get())     
        } 
        getInvertedMatrix <- function(){
                mt      
        } 
        
        list(set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix) 
}


##Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        mt <- x$getInvertedMatrix()
        if(!is.null(mt)) {
                message("getting cached data")
                return(mt)
        }
        data <- x$get()
        mt <- solve(data)
        x$setInvertedMatrix(mt)
        mt
}
