## The makeCacheMatrix function take the matrix 'x' and creates an object that
## can cache inverse of the object x.
## It enables the user display elements of the matrix and their inverse using the get() and
## getInverse function.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) 
    inv <<- inverse
  
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse of x is available in the cache,the cachesolve retrieves if from there.

cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()       
  
  if (!is.null(inv)) 
  {
    message("Retrieving data from cache")
    return(inv)     ## Returning  inverse-matrix of 'x'
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv         ## Returning  inverse-matrix of 'x'
}


##----------------Executing the program----------------------

## making a 3x3 matrix
my_matrix <- makeCacheMatrix ( matrix(1:9, 3, 3) ) 

## Initializing the 3x3 matrix
my_matrix$set( matrix( c( 5, 10, 15, 22,25,30, 35, 40, 45 ), 3, 3) )

## Displaying elements in the matrix
my_matrix$get()

## Displaying inverse of the matrix, before calling the cacheSolve function.
my_matrix$getInverse()

## Displaying inverse of the matrix, after calling the cacheSolve function.
cacheSolve(my_matrix)
my_matrix$getInverse()
