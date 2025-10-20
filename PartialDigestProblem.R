Remove <- function(deltaX, deltaY) {
  for (d in deltaY) {
    idx <- match(d, deltaX)
    if (!is.na(idx)) {
      deltaX <- deltaX[-idx]
    }
  }
  return(deltaX)
}

Place <- function(deltaX, X, width){
  if (length(deltaX) == 0){
    cat("Positions of the restiction sites:", sort(X), "\n")
    return()
  }
  
  y <- max(deltaX)
  deltayX <- abs(y - X)
  
  if (all(deltayX %in% deltaX)){
    X <- c(X, y)
    deltaX <- Remove(deltaX,deltayX)
    Place(deltaX, X, width)
    
    X <- X[-length(X)]
    deltaX <- c(deltaX, deltayX)
  }
  
  deltawyX <- abs((width - y) - X)
  if (all(deltawyX %in% deltaX)){
    X <- c(X, (width - y))
    deltaX <- Remove(deltaX, deltawyX)
    Place(deltaX, X, width)
    
    X <- X[-length(X)]
    deltaX <- c(deltaX, deltawyX)
  } 
  return()
}


PartialDigestProblem <- function(deltaX){
  width <- max(deltaX)
  deltaX <- deltaX[deltaX != width]
  X <- c(0, width)
  Place(deltaX, X, width)
}

deltaX <- c(2,2,3,3,4,5,6,7,8,10)
PartialDigestProblem(deltaX)
