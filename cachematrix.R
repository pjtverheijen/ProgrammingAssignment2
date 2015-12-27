makeCacheMatrix <- function(x = matrix()) {
  dims <- dim(x)
  if (length(dims)!=2 || dims[1]!=dims[2]) {
    stop("makeMatrix: input not a square matrix")
  }
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv,r) {
    i <<- inv
    if (r<dim(x)[1]){
      message("rank is insufficient. Using pseudo-inverse")
    }
  }
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## simple solution is: i <- solve(data)
  ## here used the svd and allow for pseudo-inverse
  svdcomp <- svd(data)
  ## rank determination based on choice matlab
  r<-svdcomp$d>svdcomp$d[1]*3e-16
  i <- svdcomp$v[,r] %*% diag(1/svdcomp$d[r]) %*% t(svdcomp$u[,r])
  x$setinv(i,sum(r))
  i
}
