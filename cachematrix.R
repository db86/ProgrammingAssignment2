## Put comments here that give an overall description of what your

## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##1. set input x: a square invertible matrix  
makeCacheMatrix <- function(x=matrix()) {
  ##2. Set an empty variable for result of inverse
    m_inverse <- NULL
    ##3. Create function of y variable to assign new input matrix
    ##4. Set matrix: Assign input matrix y to x. x is a free variable, in an environment different from current environment
    set <- function(y){
        x <<-y
    ##5. Set the result to NULL  
        m_inverse <<- NULL
        }
  ##6. Get the matrix: from function of x
  get <- function() x 
  ##7. Set the inverse matrix: inverse of actual matrix in variable "m_inverse"
  setinverse <- function(inverse) m_inverse <<- inverse
  ##8. Get the inverse matrix: output from variable "m_inverse"
  getinverse <-  function() m_inverse
  ##9. Set list for all functions used
  list(set = set, get = get,
       setmean = setinverse,
       getmean = getinverse) 
}

#-----------------------------------------------

## Write a short comment describing this function
##CacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

##1: x as the output from makeCacheMatrix
cacheSolve <- function(x,...){
  ##2: Returns inverse of original matrix input to makeCacheMatrix()
  m<-x$getinverse()
  ##3: Checks if the matrix is already calculated
  if(!is.null(m)){
    ##4: Returns value from Cache and skips the computation; returns m value
    message("getting cached data")
    return(m)
    }
  ##5: Else gets the new matrix input using "get" function
  data <- x$get()
  ##6: Calculates inverse using "solve" function
  m <- solve(data,...)
  ##7: Sets the new result to variable "m_inverse" in "makeCacheMatrix()"
  x$setinverse(m)
  ##8: Returns the new results as m
  m
  }
  
#---------------------------------------
