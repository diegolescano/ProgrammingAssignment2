## Put comments here that give an overall description of what your
## functions do
#These functions are intented to calculate the inverse of a matrix and cache its value so there is no need to calculate the inverse value everytime.

## Write a short comment describing this function
#This function introduces functions to set and get the value of the matrix and its cached inversed value.
makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL                         #sets the value of inv as NULL
      
    set <- function(y) {                   
        x <<- y                         
	    inv <<- NULL
    }
		    
    get <- function(){
        x                               #gets the value of x
    } 
			      
    set_inverted_matrix <- function(inverted_matrix){        #it receives the calculated inverse of the matrix
        inv <<- inverted_matrix                      #caches the value in inv
    } 
				        
    get_inverted_matrix <- function() inv           #returns the value of the inverse matrix
					    
    list(set = set, get = get,
         set_inverted_matrix = set_inverted_matrix,
         get_inverted_matrix = get_inverted_matrix)
}

## Write a short comment describing this function
#This function tries to get the cached inversed value of the matrix, if the value is not yet cached, it calculates the inversed value and caches it. Then returns the value.
cacheSolve <- function(x, ...) { #first called. It accepts the name of the function, i.e. makeCacheMatrix
  
    inv <- x$get_inverted_matrix()          #make a call to the get_inverted_matrix function in makeCacheMatrix and gets the value. It stores the value in inv variable
      
    if(!is.null(inv)) {                  #check if the value of inv is not null.
        message("getting cached data")     #if it's not null, then gets the cached data
        return(inv)                        #returns the value of inv
    }
		     
    data <- x$get()                    #get the value of the vector to calculate the inverse. Then saves the vector in data
		         
    inv <- solve(data)                #calculates the inverse of the data and save the value in inv
			     
    x$set_inverted_matrix(inv)        #passes the value of inv to the set_inverted_matrix function
			         
    inv                                  #return the value of inv
}

