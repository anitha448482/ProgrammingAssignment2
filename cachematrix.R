## makeCacheMatrix function returns the inverse of a matrix by using the cacheSolve function
## The caching if the inverse of a matrix avoids performing this costly opertation everytime 
## and enhances performance
## The caching happens in two parts:
##      1. makeCacheMatrix: Composes the matrix object and various methods to get and set the cache objects
##      2. cacheSolve: Calculates the inverse if not already calculated for the matrix i.e. matrix object is not new
## Write a short comment describing this function


## makeCacheMatrix function, takes x as the matrix that is to be inversed. It is initialised as an empty matrix
## Step 1: of the function: m is initialised to NULL
## Step 2: the set function takes y as an argument and sets its value to x in the parent environment
## Step 3: m is again re-initialised to NULL in its parent environment
## Step 4: the get function returns the value of x
## Step 5: the setinverse function assigns the matrix inverse to the value of m in the parent environment
## Step 6: the getinverse function returns m that contains the inversed matrix
## Step 7: Creates a named list is created to easily access the various functions(getters and setters) of the inversed

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve function calculates the inverse of the matrix and caches it. When asked to inverse the matrix again,
## it always returns the inverse from the cache rather than calculating the value everytime
## Step 1: The function takes x as an argument, where x is expected to be an object of makeCacheMatrix. 
##         If any other object i.e vector or a matrix is passed, the function will fail, since it needs the specific
##           values like the getter, setter functions available in its environment. These are values that we have 
##           defined in the makeCacheMatrix environment and shall not be available within any other data type
## Step 2: Retrieve the inverse of the matrix using the getinverse function that will be available under object x
## Step 3: check if that value of m is NULL, if no it means that the inverse has already been calculated for that object
## Step 4: let the user know that the inverse is being retrived from cache by printing "getting cached data"
## Step 5: simply return the matrix inverse
## Step 6: if inverse not already present then the get() function is executed to return the true matrix object
## Step 7: the solve function that inverses the matrix is executed and assigned to m
## Step 8: the setinverse function is called to set the inversed matrix value at the parent level
## Step 9: inverse is printed on console

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

