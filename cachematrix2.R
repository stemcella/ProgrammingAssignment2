## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #m.Cache is cache data containing matrix.
        m.Cache<- NULL
        set<-function(y) {
                x<<-y
                m.Cache<<-NULL
        }
        get<-function()x
        setmatrix<-function(solve) m.Cache<<-solve
        getmatrix<-function() m.Cache
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
        m.Cache <- x$getmatrix()

        #checking whether the cache is empty.
        if(!is.null(m.Cache)) {
                message("getting cached data")
                return(m.Cache)
        }
        matrix <- x$get()
        m.Cache <- solve(matrix, ...)
        x$setmatrix(m.Cache)
        m.Cache
        ## Return a matrix that is the inverse of 'x'
}
