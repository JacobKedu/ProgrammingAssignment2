## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 
	setMatrix <- function(v) { x <<- v;	i <<- NULL } 
	getMatrix <- function() x  
	cacheInverse <- function(s)	i <<- s 
	getInverse <- function() i 
	list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse() 
	if(!is.null(i))
	{ 
		message("The Cached Data") 
		return(i) 
	} 
	data <- x$getMatrix() 
	i <- solve(data) 
	x$cacheInverse(i) 
	i 		
}
## a <- makeCacheMatrix( matrix(c(5,25,88,148), nrow = 2, ncol = 2) );
## a$getMatrix();
##       [,1] [,2]
## [1,]    5   88
## [2,]   25  148
## cacheSolve(a)
##          [,1]         [,2]
## [1,] -0.10136986  0.060273973
## [2,]  0.01712329 -0.003424658
## cacheSolve(a)
## The Cached Data
##          [,1]         [,2]
## [1,] -0.10136986  0.060273973
## [2,]  0.01712329 -0.003424658
