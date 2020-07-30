## This function creates a special "matrix" object that can cache 
##its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #initializes x as an argumento and m as an objeto inside the environment
    m <- NULL
    #x takes the value of y in the parent environment
    #creates a m object in the parent environment
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    #uses the logical scoping and calls the x stored inside the environment
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         # gives the name 'set' to the set() function defined above
         setinverse = setinverse,
         getinverse = getinverse)
}
  

##This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' if it is stored
  
  m <- x$getinverse()
  # does not calculate the cached data again
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Return a matrix that is the inverse of 'x' it calculates it if not stored
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


# data used as an example
a<- matrix(data= c(1,0,1,0,2,0,-1,0,1),nrow = 3,ncol = 3)
a
#result expected
inversa_a<- solve(a)
inversa_a

#store matrix 
mymatrix<-makeCacheMatrix(a)

#calculate the inverse and store the result
cacheSolve(mymatrix)






