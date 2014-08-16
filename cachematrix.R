## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        solved <-NULL
        
        # remove set opration. not required.
        get <-function() x
        
        # caching result.
        setsolved <- function(solve) solved <<- solve
        # return cached result.
        getsolved <- function() solved
        
        # return functions as list.
        list( get = get, setsolved=setsolved, getsolved=getsolved)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    solved <-x$getsolved()
    # x shall be a global variable.
    if(!is.null(solved)){
        # second invocation of the same global variable x will reach here.
        message("Used cached result.")
        solved
    }
    
    #cache missed come here and all the hardwork starting here.
    raw <- x$get()
    solved <- solve(raw,...)
    
    # saving result to cache
    x$setsolved(solved)
    return(solved)
    
}
