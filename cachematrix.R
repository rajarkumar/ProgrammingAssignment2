## Put comments here that give an overall description of what your
## functions do
## 1.  set the value of the matrix 
## 2.  get the value of the matrix 
## 3.  setinv function sets the value of the matrix inverse 
## 4.  getinv function gets the value of the matrix inverse 

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) 
{ 
        # initialize the mat object
        mat <- NULL 
        
        # Gets the input matrix and save it to object x AND initialize the mat object to Null
        set <- function(imat) 
        { 
                x <<- imat
                mat <<- NULL 
        } 
        # This function returns the original matrix
        get <- function() 
        { 
                x 
        }
        # This function sets the inverse of the matrix.
        setInverse <- function(Inverse) 
        { 
                mat <<- Inverse 
        }
        # This function returns the inverse of the matrix.
        getInverse <- function() 
        {
                mat 
        }
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse) 
}


## Write a short comment describing this function
## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix above.  
## if inverse has calculated already, then cacheSolve retrieve the inverse from the cache otherwise it calculates the 
## inverse of the matrix and set the inverse of the matrix.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse() 
        
        #check null to return cached data 
        if(!is.null(mat)) 
        { 
                message("getting cached data") 
                # Return the inverse of the matrix from cache.  This occurs when the function is being called 2nd time.
                return(mat) 
        } 
        
        # Get the matrix object 
        data <- x$get() 
        
        #solve function creates inverse of the matrix 
        mat <- solve(data, ...) 
        
        #Store the Inverse of the matrix 
        x$setInverse(mat) 

        # Return the Inverse of the matrix
        mat 
        
}
