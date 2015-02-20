## Calculating inverse of the matrix can be a computational
## heavy operation. Especially if matrix is bigger. 
## Following methods compute the matrix inverse, store them in
## cache and subsequent calls check the matrix inverse value in 
## cache. If available, "cached" value is returned skipping 
## complex calculations. It is assumed that matrix supplied is
## always invertible



## {makeCacheMatrix} function takes a square matrix as an 
## argument. It returns a list which consists of functions to
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set inverse of the matrix
## 4. get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) mInv <<- inv
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## {cacheSolve} function takes "cacheMatrix" as an argument
## It first checks whether inverse of the matrix is present
## if yes, it returns the "inverse" from cache.
## else, it calculates the "inverse" of the matrix and 
## sets it's value  by calling "x$setInv()".
## It also returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        invX <- x$getInv()

        if (!is.null(invX)) {
             message("getting cached matrix data.")

        } else {
             d <- x$get()
             invX <- solve(d,...)
             x$setInv(invX)
        }
   invX
}
