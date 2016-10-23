## 2 functions: first will create a special matrix that can cache its inverse
## second function will either compute the inverse of the special matrix or retrieve
## it from cache if it already exists.
## 
## 

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              m<-NULL
              set<-function(y){
                    x<<-y
                    m<<-NULL
                
                
              }
              get<-function()x
              setsolve<-function(solve) m<<-solve
              getsolve<-function()m
              list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
  
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix function. If the inverse has already 
##been calculated and there were no changes to it, 
##then the cachesolve will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m<-x$getsolve()
          if(!is.null(m)){
                message("just one moment, working on getting cache data")
                return(m)
            
          }
          data<-x$get()
          m<-solve(data,...)
          x$setsolve(m)
          m
  
}
