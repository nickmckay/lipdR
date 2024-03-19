#' handle NAs in cbind
#'
#' @param ... whatever you're binding
#' @param fill unused parameter
#'
#' @return bound columns
#' @export
cbind.NA <- function (..., fill = NA)
{
  DFx <- list(...)
  for (i in 1:length(DFx)){
    if (is.null(dim(DFx[[i]]))){
      DFx[[i]] <- as.data.frame(DFx[[i]])
      next()
    }
    if (dim(DFx[[i]])[1] == 0){
      DFx[[i]] <- data.frame(rep(NA, 1))
    }
  }

  vertLen <-  function(dfIn){
    if(is.null(dim(dfIn))){
      vertical <- length(dfIn)
    }else{
      vertical <- length(dfIn[,1])
    }
    return(vertical)
  }

  lengthsDF <- as.numeric(lapply(DFx, vertLen))
  maxLen <- max(lengthsDF)
  newDF <- rep(NA, maxLen)
  insideDF <- NULL
  for(i in 1:length(DFx)){
    insideDF <- NULL
    if(lengthsDF[i] < maxLen){
      padLen <- maxLen - lengthsDF[i]
      padding <- data.frame(matrix(nrow =  padLen, ncol = dim(DFx[[i]])[2], data = NA))
      names(padding) <- names(DFx[[i]])
      insideDF <- rbind(DFx[[i]], padding)
    }else{
      insideDF <- DFx[[i]]}
    newDF <- cbind(newDF, insideDF)
  }
  newDF <- newDF[,-1]
  return(newDF)
}



mergeIN <- function(dfIn, colNum = 1){
  maxBy <- NULL
  minBy <- NULL
  for (i in 1:length(dfIn)){
    newMax <- max(as.numeric(dfIn[[i]][,colNum]))
    newMin <- min(as.numeric(dfIn[[i]][,colNum]))
    if (!is.null(maxBy)){
      if (newMax > maxBy){
        maxBy <- newMax
      }
    }else{
      maxBy <- newMax
    }
    if (!is.null(minBy)){
      if (newMin < minBy){
        minBy <- newMin
      }
    }else{
      minBy <- newMin
    }
  }
  newBy <- c(minBy, maxBy)
  return(newBy)
}



