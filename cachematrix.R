## This is a function factory which will make a special "matrix" object
## This object with be used to 'cache in memory' an inverse matrix.
## The benefit will be obtained when the 'cached object' is called numerous times.
## instead of being re-computed repeatedly.
##
## Strategy of a 'function builder'caching in memory' is trading memory for execution time
##
## Executing on 1st call is longer and more memory is used
## but accessing memory will be less consuming than recomputing
## Reference can be found in "Advanced R by Hardley Wickham
## at http://adv-r.had.co.nz/Environments.html

## The 1st function function builds the special "matrix" object
## and contains is a list of 4 functions commented in place
## The result is that:
## 'the enclosing environment of the child function cacheSolve() is the execution environment of 
## the parent function makecacheMatrix(), and is no longer ephemeral.'

makeCacheMatrix <- function(x = matrix()) {
        ## 0. initialization
        m <- NULL
        ## function 1. set the value of the matrix() object
        set <- function(y) {
                ## <<- is called a deep assignment arrow
                ## this is used to modify an existing variable created
                ## found by 'walking up' the parent environments
                ## This allows persistence (non-ephemeral) states for both 
                ## the m and x objects
                x <<- y
                m <<- NULL
        }
        ## function 2: get the value of the matrix() object
        get<-function() x
        ## function 3: set the matrix inverse by solve()
        ## again, the <<- deep assignment is used to preserve the m matrix solved
        ## in a persistent state across parent/child environments
        setmatrix<- function(solve) m<<- solve
        ## function 4: get the matrix inverse from memory
        getmatrix<- function() m
        ## implement the list of the 4 functions
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getmatrix()
        ## m<>NULL on all except the 1st call
        if(!is.null(m)) {
                ## we can retrieve from cache since we computed
                ## it on 1st call and m is the inverse matrix result
                message("getting cache data")
                return(m)
        }
        ## this is our 1st time thru, so we must obtain the matrix
        ## that is provided from the function factory get() cohersed
        ## to a matrix object
        matrix<-x$get()
        ## invoque the R-built-in solve() function to inverse the matrix
        m<-solve(matrix,...)
        ## pass the function back to the parent environment
        x$setmatrix(m)
        ## and return the matrix inverse
        m
}
