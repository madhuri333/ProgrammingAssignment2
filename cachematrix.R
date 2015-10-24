###########################################################################################################################################################
###########################################################################################################################################################
############################################################# CACHING INVERSE OF A MATRIX #################################################################
##################################################################### TWO FUNCTIONS #######################################################################
############################## First function is makeCacheMatrix(). It creates a matrix object that can cache its inverse. ################################
############################## Second function is cacheSolve(). It checks if inverse of matrix in above mentioned matrix object ###########################
############################## is available. If yes, retrieves it from cache and returns. Else computes it and saves it for future ########################
###########################################################################################################################################################
###########################################################################################################################################################

# makeCacheMatrix function  creates a special matrix object which can cache (save) its inverse for future access
makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        setMatrix <- function(matrix1){         # setMatrix() function takes the input matrix (matrix1) and assigns it to a matrix object "x". 
                x <<- matrix1                   # "x" is now availabl in makeCacheMatrix (parent environ of setMatrix) & can be accessed by other func in it 
                inv <<- matrix()                # "inv" matrix object (in makeCacheMatrix) is set to be NULL matrix 
        }
        
        getMatrix <- function(){x}              # returns the matrix "x"
        
        setinv <- function(inverseMatrix){      # "inv" matrix is set to "inverseMatrix"
                inv <<- inverseMatrix
        }
        
        getinv <- function() {inv}              # returns the cached inverse matrix "inv"
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setinv = setinv, getinv = getinv)    # makeCacheMatrix function returns list of all functions
}


# cacheSolve function checks if there is an inverse matrix already available for "m" matrix passed as argument to it. 
# If yes, it retrieves and returns it. Else it calculates it, saves it and returns it.

cacheSolve <- function(m,...){
        
        inverse1 <- m$getinv()                     # first retrieves inverse matrix corresponding to "m"
        if(!is.null(inverse1)){                    # if it is not NULL, returns it.
                message("Getting cached data")
                return(inverse1)
        }
                                                  
        cachedMatrix <- m$get()                    # if inverse1 is NULL, the matrix for "m" object is retrieved 
        inverse1 <- solve(cachedMatrix,...)        # no check for invertibility since it is instructed to be assumed. Inverse of matrix is computed. 
        m$setinv(inverse1)                         # saves this inverse matrix to "m" for future
        inverse1                                   # returns the inverse matrix
}
