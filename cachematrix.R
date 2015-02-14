## This is a function factory which will make a special "matrix" object
## This object with be used to 'cache in memory' an inverse matrix.
## The benefit will be obtained when the 'cached object' is called numerous times.
## instead of being re-computed repeatedly.

## The 1st function makeCacheMatrix builds the special "matrix" object
## and contains is a list of 4 functions commented in place

makeCacheMatrix <- function(x = matrix(),...) {
        ## 0. initialization
        inv <- NULL
        ## function 1. set the value of the matrix() object
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## function 2: get the value of the matrix() object
        get<-function() x
        ## function 3: solve the inversematrix
        setinversematrix<- function(solve,...) inv<<- solve
        ## function 4: get the inversematrix
        getinversematrix<- function() inv
        ## make the list to contain the 4 names and function pairs
        list(set=set, get=get,
             setinversematrix=setinversematrix,
             getinversematrix=getinversematrix)
}
## The cacheSolve() function is implemented to substitute the solve() function
## its input is a list of 4 functions implemented in makeCacheMatrix()
## its ouput is the inversematrix inv

cacheSolve <- function(x, ...) {
        ## Return the inverse matrix  of 'x' by calling getinversematrix function
        inv <-x$getinversematrix()
        ## inv<>NULL on all except the 1st call
        if(!is.null(inv)) {
                ## we can retrieve the inversematrix from cache
                message("getting inversematrix from cache")
                return(inv)
        }
        ## this is our 1st time thru, so we must get the matrix
        data<-x$get()
        ## invoque the R-built-in solve() function to inverse the matrix
        inv<-solve(data,...)
        ## pass the function back to the parent environment
        x$setinversematrix(inv)
        ## and return the matrix inverse
        inv
}

##      Example of useage
##
##      >MyMatrix <-makeCacheMatrix(matrix(1:4,2,2))
##      >cacheSolve(MyMatrix)
##      > cacheSolve(myMatrix)
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > cacheSolve(myMatrix)
##      getting inversematrix from cache
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > 