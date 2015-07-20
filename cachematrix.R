##This fct is in parallel to the makeVector fct
## (except for inv of matrix instead of mean of vector)
## It does the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inv
## 4. get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv <<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## The following function calculates the inverse of 
## the special "matrix" created with the above function. 
## it first checks to see if the inverse has already been calculated. 
##If so, it gets the inv from the cache and skips the computation. 
## Otherwise, it calculates the inv of the data and sets the value 
## of the inv in the cache via the setinv function.
## 
## Reminder: we are allowed to assume that the matrix has an inverse
## so no test for that done
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}

## for testing display time it takes to inverse random 1000x1000 matrix &
## time it takes to retrive from cache
testcaching<-function(){
  set.seed(07192015)
  r = rnorm(1000000)
  mat = matrix(r, nrow=1000, ncol=1000)
  testmat = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(testmat)
  comptime = Sys.time() - start.time
  message("Time when calculating inverse:")
  print(comptime)
  
  start.time = Sys.time()
  cacheSolve(testmat)
  comptime = Sys.time() - start.time
  message("Time when retriving inverse from cache")
  print(comptime)
}