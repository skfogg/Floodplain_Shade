## Increment Dim adds a 'column' to any dimension of a matrix or array
## x is the matrix or array, dimIDX is the dimension to be incremented,
## newVals contains the values to the 'column' to be added (and will be 
## recycled). Increment is the column name. 

## REQUIRES RLANG PACKAGE TO BE INSTALLED BUT NOT LOADED

## EXAMPLE USAGE
# test <- matrix(1:20, nrow = 5, dimnames = list(letters[1:5], LETTERS[1:4]))
# test
# incrementDim(test, dimIDX = 2, newVals = 101:105, incrementName = "X")
# incrementDim(test, dimIDX = 1, newVals = 1, incrementName = "x")

incrementDim <- function(x, dimIDX, newVals,incrementName = ""){
  # Make a vector of length to add to each dimension
  increment <- rep(0, length(dim(x)))
  increment[dimIDX] <- 1
  
  # Make an array with new dimension lengths
  newx <- array(NA, dim = dim(x) + increment)
  newdimnames <- dimnames(x)
  newdimnames[[dimIDX]] <- c(newdimnames[[dimIDX]], incrementName)
  dimnames(newx) <- newdimnames
  
  # Make a list of missing arguments
  dimlist <- lapply(1:length(dim(x)), function(y) rlang::expr())
  
  # Replace missing arguement for requested dimension
  dimlist[[dimIDX]] <- -dim(newx)[dimIDX]
  
  # Copy values from original array into new array
  newx <- do.call(`[<-`, c(list(newx), dimlist, list(x)))
  
  # Copy new values into new array
  dimlist[[dimIDX]] <- -dimlist[[dimIDX]]
  newx <- do.call(`[<-`, c(list(newx), dimlist, list(newVals)))
  return(newx)
}
