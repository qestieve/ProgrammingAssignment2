## Bellow functions allow to cache the inverse of a matrix object
## into an 'extended' matrix x. Different interaction with x are available:
## x$set and x$get to set and return the matrix. x$setinverse and x$getinverse 
## to set and return the inverse of the matrix.


# this function produces the extended matrix object x and defines
# availble operation on x. The list of those is return at the end of the function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL #clear inv if previously assigend
    set <- function(y) { # Function which set the value of the matrix
                x <<- y #set the matrix
                inv <<- NULL # clear inv 
    }
    get <- function() x # Function which return the value of the matrix 
    setinverse <- function(inverse) inv <<- inverse # Function which set the value of the inverse of matrix 
    getinverse <- function() inv # Function which returns the value of the inverse of matrix
    
	#return a list with the available functions
	list(set = set, get = get,
    setinverse = inverse,
    getinverse = getinverse)
}


# this function check if the inverse of the matrix is cached. If yes,it returns the inverse matrix.
# Otherwise, it computes the inverse of the matrix. This fuction take as parameter an object 
# produced by the function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
		#check if the inverse of the matrix have been already cached
        if(!is.null(inv)) { 
				#if yes return the invesed matrix
                message("getting cached data")
                return(inv)
        }
        #if no cahed invere matrix
		data <- x$get() #get the matrix
        inv <- solve(data) # inverse the matrix
        x$setinverse(inv) #cache the inverse into x
        print(inv) #print the inverse 
        ## Return a matrix that is the inverse of 'x'
}
