## a pair of functions that cache the inverse of a matrix.


## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        # above, x and inverse are initialised as objects in the makeCacheMatrix
        # environment for use later
        
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        # assign the input argument to the x object in the parent environment
        # Assign the value of NULL to the inversematrix object in the parent environment
        # this line of code clears any value for inversematrix that had been cached 
        # by a prior execution of CacheSolve
        #therefore whenever x is reset, the cached value of inversematrix is cleared
        # this forces subsequet calls to cacheSolve to recalculate the inversematrix
        # rather than retrieving it from the cache
        
        get <- function() x
        # here makeCacheMatrix defines the getter for x
        # since x is not defined within get(), R retrieves it from the parent
        # environment of makeCacheMatrix
        
        setsolve <- function(solve) inversematrix <<- solve
        # makeCacheMatrix defines the setter for the inversematrix
        # since inversematrix is defined in the parent environment and it needs to
        # be accessed after setinversematrix() completes, the code uses the <<-
        # form of the assignment operator to assign the input argument to the 
        # value of inversematrix in the parent environment
        
        getsolve <- function() inversematrix
        # finally makeCacheMatrix defines the getter for the inversematrix
        # since inversematrix is now in the environment of getsolve, <- is used
        # to search for inversematrix
        
        # now getters and setters have been defined for both of the data objects within
        # the makeCacheMatrix object
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
        # The last section of code assigns each of these functions as an element 
        # within a list(), and returns it to the parent environment.
        # so returns a fully formed object of type makeCacheMatrix() 
        # to be used by downstream R code. 
        # each element in the list is named. 
        
        # That is, each element in the list is created with a elementName = value syntax
        # this allows use of the $ form of the extract operator to access the
        # functions by name rather than using the [[ form of the extract operator, 
        # to get the contents of the vector
}



## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # cacheSolve() starts with a single argument, x, and an ellipsis
        # that allows the caller to pass additional arguments into the function
        inversematrix <- x$getsolve()
        # function attempts to retrieve an inversematrix from the object 
        # passed in as the argument. 
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
# Then it checks to see whether the result is NULL
# Since makeCacheMatrix() sets the cached matrix to NULL
# whenever a new matrix is set into the object then if the value here != NULL
# that means that we have a valid, calculated inversematrix in the parent environment
        }
        
        # if the result is false then the below code gets the matrix from the
        #  input object, it calcs the inversematrix and sets that as the 
        # inversematrix of the input object, it then returns the value of the  
        # inversematrix to the parent environment by printing the solve object.
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setsolve(inversematrix)
        inversematrix
}
