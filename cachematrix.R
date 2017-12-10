##Using the same framework for caching as above we create a list of functions
makeInverse <-function(x = matrix()) {
  i <-NULL #Sets the inverse to null (signals it hasn't been computed)
  set <-function(y) {
    x <<-y ##Stores called matrix outside of current environment
    i <<- NULL #Resets inverse to null each time the called matrix is changed
  }
  get <-function() {x} ##Returns the called matrix
  setsolve <-function (solve) {i <<- solve} ##Stores inverse outside of current environment
  getsolve <-function() {i} ##Returns the Inverse already computed
  list(set = set,get = get,
  setsolve = setsolve,
  getsolve = getsolve)

}

##Function assumes that
cachesolve <-function(x, ...) {
  i <- x$getsolve()  #Retrieves the inverse from list
  if(!is.null(i)) {
    message("retrieving inverse")
    return(i) ##If i is not null then it returns it
    
  }
  data <-x$get() #otherwise the called matrix is retrieved from the list
  i <- solve(data, ...)#the inverse is computed
  x$setsolve(i) #and then cached in the list
  i#and returned to the user
}

##Testing

x1 <- makeInverse(matrix(c(1,2,3,4),2,2))
x1$getsolve() #Inverse not computed yet
cachesolve(x1) #inverse returned after computation
cachesolve(x1) #inverse returned from cache
x1$set(x1$getsolve()) #Setting the function call to be the computed inverse
cachesolve(x1) #Inverse of the inverse is the original matrix
