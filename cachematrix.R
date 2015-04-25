## Functions that will cache the inverse of a matrix

##  makeVectorMatrix creates a matrix 

makeCacheMatrix <- function(x = matrix())
 {
     ins <- NULL
     set <- function(y) 
     {
         x <<- y
         ins <<- NULL
     }
     
#getting the value of the matrix
#setting the value of the matrix
#getting the inverse value of the matrix
#setting the inverse value of the matrix
     
     get <- function()x  
     setinverse <- function(inv) ins <<-inv 
     getinverse <- function() ins
    
     list (
           set=set,
           get=get,
           setinverse=setinverse,
           getinverse=getinverse
          )
 }

## cacheSolve function will calculate the inverse square matrix getting input from the above funtion 

cacheSolve <- function(x, ...)   
{
     ins <- x$getinverse()
 
       if(!is.null(ins)) 
         {
            message("getting cached data")
            return(ins)
         }
 
     dat <- x$get()
     
#solve() will give square invertible matrix
     ins <- solve(dat,...) 
     x$setinverse(ins)
     ins

}


#Calling the functions

#input, matrix

#Example1
 dat <- makeCacheMatrix(matrix(c(4,3,1,1), c(2, 2)))
 cacheSolve(dat)
 
 #Output , Inverse matrix result
 
     [,1] [,2]
[1,]    1   -1
[2,]   -3    4


#Example2
dat <- makeCacheMatrix(matrix(c(3,6,2,4), c(2, 2)))
cacheSolve(dat)

#Output , Inverse matrix result

 Error: Lapack routine dgesv: system is exactly singular: U[2,2] = 0
 Because the determinant is zero the matrix is singular and no inverse exists.
 

