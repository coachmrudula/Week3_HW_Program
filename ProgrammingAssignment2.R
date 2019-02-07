## Put comments here that give an overall description of what your
 ## functions do
   
   ## The makeCacheMatrix function takes matrix as an input argument.
   ## The functions assigns/sets a value of the matrix and its inverse.
   ##The function also demonstartes the use of <<- operator. This operator in R is used to
  ## to assign a global value to a variable
  
   makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL                             ## initialize variable inv as NULL
        set <- function(y) {                    
            x <<- y                               
            inv <<- NULL      ## for new matrix, set inv to NULL
            
        }
        
        
        get <- function() x                     ## return value of the matrix argument
        
        setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
        getinverse <- function() inv                     ## gets the value of inv where called
        list(set = set, 
             get = get, 
            setinverse = setinverse, 
            getinverse = getinverse) 
          
          
        
   }   
  
    ## the program below takes the output of the makeCacheMatrix function as an input variable
    ##After checking if there is existing value in cached data or not, this fuction
    ## then uses the "solve " function to set the inverse matrix.
    
   cacheSolve <- function(x, ...)  {
             ## Return a matrix that is the inverse of 'x'
         
            inv <- x$getinverse()
            
             if(!is.null(inv)) {
                 message("getting cached Matrix data ")
                 return(inv)
             }
            
            ## If the value of the matrix is NULL then use the solve function 
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
            
      }
    


