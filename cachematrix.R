########################################################################
## This file contains 2 functions which demonstrate caching data using
## R's Scoping Rules (closures).
##
## This file also contains a 3rd function which is a simple unit test.
##
## Functions:
## makeCacheMatrix: Holder for a matrix and its inverse
## cacheSolve:      Helper function for accesing inverse matrix
## unitTest:        A primitive unit test function
##########################################

##########################################
## Function:   makeCacheMatrix()
## Returns:    List of public functions - set/get matrix and its inverse
## Note:       This function uses a nested function to cache the inverse
##             matrix using a closure.
##
## Usage:      myMat <- makeCacheMatrix(<some matrix>)
##
## Public Functions:
## get:        Returns the enclosed matrix
## set:        Sets the enclosed matrix and sets invers to NULL
## getInverse: Returns the inverse matrix
## setInverse: Sets the enclosed inverse matrix
##########################################
makeCacheMatrix <- function(x = matrix()) {
        
        ## Data for this function
        xInverse <- NULL             ## Cached matrix INVERSE
        
        ## Validate function argument
        if (is.null(x)) stop("Invalid function argument (x) is NULL" )
        
        ## Setter for MATRIX
        set <- function(aMatrix) {
                x <<- aMatrix         ## Matrix we're encapsulating
                xInverse <<- NULL     ## Initialize inverse to NULL
        }
        
        ## Getter for MATRIX
        get <- function() x
        
        ## Setter for INVERSE
        setInverse <- function(anInverse) xInverse <<- anInverse
        
        ## Getter for INVERSE
        getInverse <- function() xInverse
        
        ## Return list of public API functions
        list(set=set, get=get, 
              setInverse=setInverse, getInverse=getInverse)
}

##########################################
## Function: cacheSolve()
## Returns:  Inverse of matrix stored in x
## Note:     This function will return inverse if already cached in x
##           or will compute the inverse if not yet computed and store
##           the inverse in x using the x$setInverse() function.
##
## Usage:    myMat <- rnorm(36, ncol = 6, nrow = 6) ## create matrix
##           myCachedMat <- makeCacheMatrix(myMat)  ## create wrapper
##           myMatInv <- cacheSolve(myMatInv)       ## get inverse
##########################################
cacheSolve <- function(x, ...) {
        
        ## Validate argument
        if (is.null(x))
                stop("Invalid function argument (x) is NULL")
        
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInverse()
        
        ## If not NULL, inverse already computed and cached
        ## So return cached inverse matrix
        if (!is.null(xInv)) {
                message("getting cached matrix inverse")
                return(xInv)
        }
        
        ## If reach here (is NULL), inverse not yet computed
        ## Compute inverse and cache its value
        xMatrix <- x$get()
        xInv <- solve(xMatrix, ...)
        x$setInverse(xInv)
        
        ## Return just-computed inverse matrix
        xInv
}

##########################################
## Function: unitTest()
## Returns:  Nothing
## Note:     A primitive unit test
## Usage:    Just run it: unitTest()
## TODO: Make more robust and programmatically check results
## TODO: Output a PASS/FAIL message at end
## TODO: Improve output formatting (learn more about R printing)
##########################################
unitTest <- function() {
        print("Running simple unit test...")
        
        m1 <- matrix(rnorm(9), nrow=3, ncol=3) ## create 6x6 matrix
        print("created 3x3 matrix:"); 
        print(m1); print("------------")
        
        print("creating makeCacheMatrix instance named 'cm'")
        cm <- makeCacheMatrix(m1)
        
        m <- cm$get()
        print("cm$get() returned matrix:"); 
        print(m); print("------------")
        
        matInv <- cm$getInverse()
        print("cm$getInverse() result (should be NULL):"); 
        print(matInv); print("------------")
        
        matInv <- cacheSolve(cm)
        print("cachceSolve(cm) result 1st time:"); 
        print(matInv); print("------------")

        print("cachceSolve(cm) result 2nd time:"); 
        matInv <- cacheSolve(cm)
        print(matInv); print("------------")
        
        print("cm$get() %*% cacheSolve(cm) (expect Identity matrix):"); 
        matProd <-cm$get() %*% cacheSolve(cm)
        print(matProd); print("------------")
        
        m2 <- matrix(rnorm(16), nrow=4, ncol=4)
        print("created 2nd matrix (m2), this one 4x4"); 
        print(m2); print("------------")
        
        cm$set(m2)
        m <- cm$get()
        print("after calling cm$set(m2), return value of cm$get():")
        print(m); print("------------")
        
        matInv <- cm$getInverse()
        print("cm$getInverse() (should be NULL):"); 
        print(matInv); print("------------")
        
        print("cacheSolve(cm) with new matrix return value (1st call):")
        matInv <- cacheSolve(cm)
        print(matInv); print("------------")
        
        print("cacheSolve(cm) with new matrix return value (2nd call):")
        matInv <- cacheSolve(cm)
        print(matInv); print("------------")
        
        print("Try identical(cacheSolve(cm), cm$getInverse()):")
        print(identical(cacheSolve(cm), cm$getInverse())); 
        print("------------")
        
        print("")
        print("End of unit test...")
}

##########################################
## Results of running unitTest()
##########################################
## > source("cachematrix.R")
## > unitTest()
## [1] "Running simple unit test..."
## [1] "created 3x3 matrix:"
##            [,1]       [,2]      [,3]
## [1,] 0.52801179 -0.3112213 0.6050826
## [2,] 0.89419720 -0.9596283 0.8336974
## [3,] 0.04724022  0.8019237 1.0091474
## [1] "------------"
## [1] "creating makeCacheMatrix instance named 'cm'"
## [1] "cm$get() returned matrix:"
##            [,1]       [,2]      [,3]
## [1,] 0.52801179 -0.3112213 0.6050826
## [2,] 0.89419720 -0.9596283 0.8336974
## [3,] 0.04724022  0.8019237 1.0091474
## [1] "------------"
## [1] "cm$getInverse() result (should be NULL):"
## NULL
## [1] "------------"
## [1] "cachceSolve(cm) result 1st time:"
##           [,1]      [,2]       [,3]
## [1,] 12.176635 -5.945603 -2.3891811
## [2,]  6.419397 -3.750934 -0.7502581
## [3,] -5.671216  3.259023  1.6989740
## [1] "------------"
## [1] "cachceSolve(cm) result 2nd time:"
## getting cached matrix inverse
##           [,1]      [,2]       [,3]
## [1,] 12.176635 -5.945603 -2.3891811
## [2,]  6.419397 -3.750934 -0.7502581
## [3,] -5.671216  3.259023  1.6989740
## [1] "------------"
## [1] "cm$get() %*% cacheSolve(cm) (expect Identity matrix):"
## getting cached matrix inverse
##              [,1]         [,2] [,3]
## [1,] 1.000000e+00 2.220446e-16    0
## [2,] 8.881784e-16 1.000000e+00    0
## [3,] 0.000000e+00 4.440892e-16    1
## [1] "------------"
## [1] "created 2nd matrix (m2), this one 4x4"
##           [,1]        [,2]       [,3]       [,4]
## [1,] 0.4745266  1.77174922 -0.8965141 -1.6313184
## [2,] 0.3405186 -1.26115186  0.6194788 -0.7334420
## [3,] 1.0990976  0.01823803 -0.6046504  0.4827727
## [4,] 0.9834310  1.65341670  0.1901167  0.1685990
## [1] "------------"
## [1] "after calling cm$set(m2), return value of cm$get():"
##           [,1]        [,2]       [,3]       [,4]
## [1,] 0.4745266  1.77174922 -0.8965141 -1.6313184
## [2,] 0.3405186 -1.26115186  0.6194788 -0.7334420
## [3,] 1.0990976  0.01823803 -0.6046504  0.4827727
## [4,] 0.9834310  1.65341670  0.1901167  0.1685990
## [1] "------------"
## [1] "cm$getInverse() (should be NULL):"
## NULL
## [1] "------------"
## [1] "cacheSolve(cm) with new matrix return value (1st call):"
##               [,1]       [,2]       [,3]      [,4]
## [1,]  0.0001665084  0.4063776  0.5117268 0.3041435
## [2,]  0.0718437477 -0.2508837 -0.2568769 0.3392934
## [3,] -0.2951309429  0.4225399 -0.5805676 0.6449514
## [4,] -0.3727309545 -0.3864844  0.1889229 0.1025299
## [1] "------------"
## [1] "cacheSolve(cm) with new matrix return value (2nd call):"
## getting cached matrix inverse
##               [,1]       [,2]       [,3]      [,4]
## [1,]  0.0001665084  0.4063776  0.5117268 0.3041435
## [2,]  0.0718437477 -0.2508837 -0.2568769 0.3392934
## [3,] -0.2951309429  0.4225399 -0.5805676 0.6449514
## [4,] -0.3727309545 -0.3864844  0.1889229 0.1025299
## [1] "------------"
## [1] "Try identical(cacheSolve(cm), cm$getInverse()):"
## getting cached matrix inverse
## [1] TRUE
## [1] "------------"
## [1] ""
## [1] "End of unit test..."
## >