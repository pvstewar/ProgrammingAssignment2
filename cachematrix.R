## This R code contains 2 functions, the first function is makeCacheMatirx and the second
## is cacheSolve, below are the steps:

## 1) The first 2 lines of the function define the function name and initialize objects
## x (a matrix) and i(a null object that will eventually contain our inverse.) 

## 2) We define our set() function on the next 3 lines, this will 
## assign input argument to x and reset our inverse to null in the parent environment.
## 
## 3) On the next line we assgn the "get" which takes advantage of lexical scoping
## in that x is not defined in the function so R knows to check in the parent
## environment for the value of x.

## 4) The next line is the "set" part of the function that will assign our argument
## (solve) to be the value of i in the parent environment.

## 5) Next line defines the "get" for the inverse i from the parent environment.

## 6) Final step of the function is to put all our functions into a list of named 
## elements which is important to ensure that the $ operator is able to access the 
## functions by name.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvrs <- function(solve) i <<- solve
        getinvrs <- function() i
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## 7) The First 2 lines define our cacheSolve function and check to see if 
## we have assigned a value to i yet.

## 8) The next 4 lines are checking whether i is null and if not it will
## simply return our cached value.

## 9) Next we get x and assign our solve function to i to assign it the inverse
## of x and place that value in our parent environment and finally we print i.

cacheSolve <- function(x, ...) {
        i <- x$getinvrs()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinvrs(i)
        i
}
