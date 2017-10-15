## This code pertains to week3's assignment in the R Programming course
## of the Data Science Specialization in Coursera

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		cacheMatrix <- NULL
		#sets the initial objects
		setInitial <- function(p_in){
			cacheMatrix <<- NULL
			x <<- p_in
		}
		
		#returns initial object
		getInitial <- function() x
		
		#returns cached object
		getCacheMatrix <- function() cacheMatrix
		
		#sets object to cache
		setCacheMatrix <- function(solve) cacheMatrix <<- solve
		
		list(setInitial = setInitial, getInitial = getInitial,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
		cacheMatrix <- x$getCacheMatrix()
		#if object exists returned cached object
        if(!is.null(cacheMatrix)) {
                message("getting cached data")
                return(cacheMatrix)
        }
		#otherwise compute inverse matrix
        firstData <- x$getInitial()
        cacheMatrix <- solve(firstData)
		
		#set object to cache
        x$setCacheMatrix(cacheMatrix)
        cacheMatrix
}
