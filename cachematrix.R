## SUMMARY:
## "makeCacheMatrix" is a function created to cache a matrix and its inverse.
## "cacheSolve" is a function created to apply the function "solve()" to a 
## matrix, after having checked whether that result is already cached.

## "makeCacheMatrix" returns a list containing 4 functions ("set", "get", 
## "setinv" & "getinv") as well as the inverted matrix ("inv") and the 
## original matrix ("x")
makeCacheMatrix <- function(x = matrix()) {
  ## Object "inv" is created to be subsequently "filled"
  inv <- NULL
  ## "set" assigns the input to the "x" object in the same environment as the 
  ## other functions (the parent environment). It can be used to change the 
  ## input. In addition, it also establishes that "inv" is an "empty" object
  ## that is also present in the parent environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## "get" defines a function to get the object "x" from the parent environment.
  get <- function() x
  ## "setinv" defines a function to feed an object to the object "inv" in the
  ## parent environment.
  setinv <- function(inverse) inv <<- inverse
  ## "getinv" defines a function to get the object "inv" from the parent
  ## environment.
  getinv <- function() inv
  ## "makeCacheMatrix" returns a list of the functions to the parent 
  ## environment.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## "cacheSolve" returns the inverse of a "makeCacheMatrix()" type argument.
cacheSolve <- function(x, ...) {
  ## Firstly, it is checked whether the "getinv" element of the 
  ## "makeCacheMatrix()" is NULL or not.
  inv <- x$getinv()
  ## If "getinv" is not NULL, it is because it is capable of retrieving "inv"
  ## from the parent environment of "makeCacheMatrix". If so, "cacheSolve" will
  ## return the previously cached object.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## If "getinv" is NULL, it means there is no calculated "inv" object.
  ## Therefore, "cacheSolve" will create a new object called "data" comprised
  ## of the object we had previously input in the "makeCacheMatrix" function.
  ## It will then calculate the inverse of the object "data" and it will call
  ## "setinv" function to feed this result to the object "inv" in the parent
  ## environment of the "makeCacheMatrix" function.
  ## Finally, "cacheSolve" also returns the "inv" object.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Test
test <- matrix(c(4, 2, 7, 6), 2, 2)
aMatrix <- makeCacheMatrix(test)
cacheSolve(aMatrix)
cacheSolve(aMatrix)
