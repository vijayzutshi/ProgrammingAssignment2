## My solution for Caching the Inverse of a Matrix:

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This is an example of Lexical Scoping in R programming language

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matinver <- NULL
  set <- function(y) {
    x <<- y
    matinver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matinver <<- inverse
  getInverse <- function() matinver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## Following three commands can be used in the console 
## to test first function - makeCacheMatrix
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get()
## my_matrix$getInverse()



## This function computes the inverse of the special matrix returned
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinver <- x$getInverse()
        if (!is.null(matinver)) {
        message("getting cached data")
         return(matinver)
        }
     matinv <- x$get()
     matinver <- solve(matinv, ...)
     x$setInverse(matinver)
    matinver
  
}

## Following three commans can be used in the console 
## to test my second function - cacheSolve 
## cacheSolve(my_matrix)
## cacheSolve(my_matrix)
## my_matrix$getInverse()

