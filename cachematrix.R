## These two functions cache the inverse of a matrix

## Function One
## Create a special matrix object to cache the inverse

makeCacheMatrix <- function(x = matrix())
{
  ## Initialize the inverse
  mat_inverse <- NULL
  
  ## Set the matrix
  set <- function( matrix )
  {
    mat_matrix <<- matrix
    mat_inverse <<- NULL
  }
  
  ## Get the matrix
  get <- function()
  {
    ## Return the matrix
    mat_matrix
  }
  
  ## Set the inverse of the matrix
  SetInverse <- function(inverse)
  {
    mat_inverse <<- inverse
  }
  
  ## Get the inverse of the matrix
  GetInverse <- function()
  {
    ## Return the inverse
    mat_inverse
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
}

## Function Two
cacheSolve <- function(x, ...)
{
  
  ## Return a matrix that is the inverse of x
  mat_inverse <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(mat_matrix) )
  {
    message("getting cached data")
    return(mat_matrix)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  mat_matrix <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(mat_matrix)
  
  ## Return the matrix
  mat_matrix
  
}
