## This is the Programming Assignment 2 of R Programming course.
## https://class.coursera.org/rprog-021/human_grading/view/courses/973977/assessments/3/submissions
##
## makeCacheMatrix and cacheSolve functions create a special objects
## that stores an inverse of a matrix. The inverse will only be calculated
## when the first time cacheSolve is called. The subsequent call of
## cacheSolve will return a cached copy of the inverse.
## 
## makeCacheMatrix creates a special "matrix", which is a list containing a fucntion to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function () m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix function.
## If the inverse has been calculated before, it will return a cached copy.
## Since any change to matrix needs to be done using set function, which will set m be NULL.
## Therefore, any change to matrix will cause the inverse being recalculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-solve(data, ...)
        x$setInverse(m)
        m
}
