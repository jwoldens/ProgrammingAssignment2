## Generally speaking, the purpose of the two fucntions is to avoid 'cosly' computing. If a particular output
##(in this case it is the inverse of a matrix) is to be reused in the futre, we can avoid computing it over
##and over again by storing the particular output in memory. In the case below, the functions (1) takes a square matrix 
##as input, (2) computes the inverse of that matrix and (3) stores the inverse in memory so that it can be 
#recalled at a later point

## Write a short comment describing this function
##Function takes a square matrix as input and then distributes it to the various sub-functions (i.e. get, etc..)
##Important to note, this function does not cacluate the inverse of the matrix. Instead, it (1) accepts the 
##input (i.e. x) and (2) sets the variable for the inverse (i.e. inverse_matrix) to null because the inverse has not 
##been calculated. The sympol <<- ensures that the value of a particluar variable transfers 'up' to an environment other
##than where it was created

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y = matrix) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(new_matrix) inverse_matrix <<- new_matrix
        get_inverse <- function() inverse_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Write a short comment describing this function
##This function contains the 'solve' function (see line...), which means that, unlike above, here we actually 
##calculate the inverse of the matrix. In short, 'cacheSolve interacts with 'makeCachMatrix by calling on the latter's
##subfunctions. Moreover, before applying the 'solve' function, it first checks whether the inverse has already 
##been calculated and stored (see line...). If not, it proceeds to get the value for x (which is the input in the 
##function above) and calculates the inverse. Lastly, it again interacts with the function above to store the 
##the new value for the inverse in 'set_inverse'. 
cacheSolve <- function(x, ...) {
        inverse_matrix <- x$get_inverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$set_inverse(inverse_matrix)
        inverse_matrix
        ## Return a matrix that is the inverse of 'x'
}
