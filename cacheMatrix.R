#Program execution instructions (in console)
# 1. Source the R file
# 2. Create a square matrix, say x <- matrix(1:9, nrow=3, ncol=3)
# 3. Use the makeCacheMatrix so that we can use the get, set, getInverse, setInverse function
# and assign it to a variable say m i.e. m <- makeCacheMatrix(x)
# 4. Use the get() method of the makeCacheMatrix to confirm that your program is running
# 5. Finally use the cacheSolve function to print the inverse of the matrix using m as an agrument
# i.e. CacheSolve(m)


#Create a matrix that can be cache for a later purpose
#we use the <<- operator for caching
makeCacheMatrix <- function(x = matrix()) {
  #inv matrix is initially set to null
  inv <- NULL
  #set is used to set the inital matrix
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  #get returns the set matrix
  get <- function() x
  #we want a function that would set the value to inv matrix using the solve function
  setInverse <- function(solve) inv <<- solve
  
  #returns the inverse matrix
  getInverse <- function() inv
  
  # i don't understand why we are using the list method here! :D
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#this matrix is used to return the inverse of the matrix, which takes the matrix made in the 
#above statement as argument
cacheSolve <- function(x, ...) {
  #value returned by getInverse method is set to inv variable
  inv <- x$getInverse()
  
  #if inv value is not null, that is it was already set, return the existing value
  if(!is.null(inv)) {
   message("getting cached data")
   return(inv)
  }
  
 #getting the matrix and setting it to data variable
 data <- x$get()
 
 #solve function is used to get the inverse of a square matrix and its value is set to inv 
 #variable
 inv <- solve(data, ...)
 
 #we want to set inverse of the matrix to inv variable
 x$setInverse(inv)
 #finally return the variable on which the inv was set
 inv
}


