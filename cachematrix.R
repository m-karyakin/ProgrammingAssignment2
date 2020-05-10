## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.
## For this purpose, a pair of functions that
## cache the inverse of a matrix is presented below.

## The first function, makeCacheMatrix, creates a special "matrix" object
## that can contain both the matrix itself and it's inverse.
## Actually this new "matrix" is a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matix
##    get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		cached <- NULL
        set <- function(y) {
                x <<- y
                cached <<- NULL
        }
        get <- function() x
        setinv <- function(inv) cached <<- inv
        getinv <- function() cached
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## The second function, cacheSolve, calculates the inverse
## of the special "matrix" created with the above function.
## It first checks to see if the inverse value has already
## been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates
## the inverse of the given matrix and sets the obtained result
## as a value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
#               message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

#-------------------------------------------Test Section

# Test 1
# Simple test of the developed functions
m <- matrix(1:4,2,2)
M <-makeCacheMatrix(m)
Y<-cacheSolve(M)
Y
Y %*% M$get()
Y<-cacheSolve(M)


# Test 2
# Can it really help increasing the speed of calculations?
# Assume we have rather big number (say, K) of square matrices nDim x nDim,
# and we need a lot of computations of the inverse of some of these 
# matrices (say, N computations, where N>>K).
# 
# Just to check if the result of two approaches are the same, we will calculate 
# the sum of the first (i.e. [1,1]) elements of all inverse matices we got. 
# For these testing purpoces you need to comment the "message" line in cacheSolve function.

list_of_matrix <- list()                 # list for storing classical matrices  
list_of_CacheMatrix <- list()            # list for storing "special" matrices
K <- 1000                                # number of matrices to analyze
N <- 5000                                # number of "experiments"
nDim <- 50                               # size of the each matrix
pos <- sample(1:K, N, replace=T)         # list of N indexes  for "experimenting"

# filling the lists with matrices
for (i in 1:K){
        # create matrix (nDim x nDim) with small random elemnts
        m <- matrix(runif(nDim*nDim, min=-1/nDim, max=1/nDim), nrow=nDim)
        # increase diagonal elements to be sure that determinant is not zero
        for (j in 1:nDim) m[j,j] <- 1
        M <- makeCacheMatrix(m)        
        list_of_matrix[[i]] <- M$get()
        list_of_CacheMatrix[[i]] <- M
}

# We have a list of K square matrices
# Let's perform N computations of the their inverse
start_time <- Sys.time()
s1 <- 0
for (i in 1:N){
        m_inv <- solve(list_of_matrix[[pos[i]]]) # calculating the inverse matrix
        s1 <- s1+m_inv[1,1] # summation of first elements to check 
                            #           if the result of two approaches are the same
}
end_time <- Sys.time()

print("Standard matrices:")
print(s1)
print(end_time-start_time)

# We have a list of K "Cache matrices"
# Let's perform N computations of the their inverse
start_time <- Sys.time()
s2 <- 0
for (i in 1:N){
        M_inv <- cacheSolve(list_of_CacheMatrix[[pos[i]]]) # calculating the inverse matrix or retrieving it from the cache
        s2 <- s2+M_inv[1,1] 
}
end_time <- Sys.time()
print("Cache matrices:")
print(s2)
print(end_time-start_time)

# As it was expected the increase of speed is proportional to the ratio N/K


