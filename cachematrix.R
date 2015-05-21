## These functions create a matrix object with get and set 
## functions, and then calculate the inverse of that matrix object

## Make the matrix object

makeCacheMatrix <- function(x = matrix()) 
{
    #initialize the inverse to be null
    
    inv<-NULL
    set<-function(y)
    {
        #set the matrix.
        ##set the value of the inverse when the matrix changes
        
        x<<-y
        inv<<-NULL
    }
    
    
    get<-function()
    {
        #fetch the matrix
        
        x
    }
    
    setinverse<-function(inverse)
    {
        #set the inverse of the matrix
        
        inv<<-inverse
    }
    
    getinverse<-function()
    {
        #fetch the inverse of the matrix
        
        inv
    }
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## calculates the inverse of the matrix object
# pulls cached data if this function has already calculated the
##mean

cacheSolve <- function(x, ...) 
{
    inv<-x$getinverse()
    if(!is.null(inv))
    {
        # check whether the inverse has already been calculated
        ## if so, return the inverse
        
        message("getting cached data")
        return(inv)
    }
    #otherwise, fetch the matrix and calculate its inverse by
    ##means of the solve function
    
    data<-x$get()
    inv<-solve(data)
    
    #set the inverse of the matrix equal to data
    
    x$setinverse(inv)
    inv
}
