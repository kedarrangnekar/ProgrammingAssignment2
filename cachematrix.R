##The functions makeCachematrix and cacheSolve respectively create a list with 4 functions
## 1. Setting the matrix
## 2. Getting the matrix
## 3. Setting/Solving for the inverse of the matrix
## 4. Getting the inverse either from Cache or recomputing it basis the fact that the matrix from previous computation is unchanged or not respectively


## "makeCacheMatrix" function creates a list of 4 functions
## 1. Setting the matrix : Takes its input matrix from the argument given by the user
## 2. Getting the matrix : Displays the input matrix
## 3. Setting inverse : Sets the inverse of the given matrix
## 4. Getting the inverse : Gets the desired inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list(set = set, get=get, setinv = setinv, getinv = getinv)
}


## "cacheSolve" function creates an inverse of a matrix if inverse is not already present (then calls inverse stored in cache) else recomputes the inverse
## The function takes an imput from the list created in the "makeCacheMatrix" and checks for inverse of the matrix being present.
## If the function is run 1st time the function computes the mean as usual.
## However on running the function a 2nd time without changing the input matrix, it checks for presence of the inverse and if found prints it with the message "getting cached data" without recomputing it
## However if user changes the matrix, the function sets the inverse to NULL and hence recomputes the same and replacing the data in cache

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        
}
