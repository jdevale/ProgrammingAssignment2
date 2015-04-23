## This file contains two functions which cache matrix inversion
## results to obviate the need for multiple calculations
## The goal was to create this functionality using R's lexical
## scoping and environment features
##
## The two basic functions are:
##
## makeCacheMatrix(), which creates a caching matrix object
## and encapsulates the original data, inverted data, and
## get/set methods for same
##
## cacheSolve(), which checks for cached data, returns it
## if available, or else it calculates the inversion,
## caches the result, and returns the result as normal



#makeCacheMatrix()  
#construct a list object which contains matrix data, its inverse (If calculated)
#and the ability to get data, set data, get inverse and set inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse<-NULL
  matrixData<-x

  #these functions form the core functionality of this object
  #They get and set the matrix data, as well as get and set its inverse
  
  #This entire construct can be thought of as a traditional "class"
  #in an production general purpose object oriented programming language
  #or as a C class with a bit of scaffolding allowing easier calls
  #of stored function pointers
  
  setMatrix<-function(data){
    matrixData<<-data
    matrixInverse<<-NULL  #setting new data, thus inverse is unknown
  }
  getMatrix<-function() matrixData 
  setInverse<-function(iData) matrixInverse<<- iData
  getInverse<-function() matrixInverse


  #Finally everything is bundled up and returned as a list.
  #All locally defined variables are part of the environment
  #the list defined functions will run within, thus class members
  #of this object are partially obfuscated from the caller, existing
  #only in the RAM based environment set up when this is created.
  list(setMatrix=setMatrix, 
       getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
}




#cacheSolve()
#Find the inverse of a given special matrix list object as defined by a call
#to makeCacheMatrix()
#If the inverse is available, it returns the cached value
#otherwise it computes the value and stores it
#this is just an interface to the CacheMatrix object created
#by makeCacheMatrix.
cacheSolve <- function(x, ...) {

  #check to see if the calculated matrix inversion is already available in the cache
  #and return it if it is
  iMatrix<-x$getInverse()
  if(!is.null(iMatrix)){
    return(iMatrix)
  }
  
  #otherwise, calculate the inversion, cache it, and return it
  matrixData<-x$getMatrix()
  iMatrix<-solve(matrixData, ...)
  x$setInverse(iMatrix)
  return(iMatrix)
}
