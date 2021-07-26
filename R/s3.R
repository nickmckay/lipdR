new_lipd <- function(x = list()){
  stopifnot(validLipd(x))
  
  structure(x,class = c("lipd",class(list())))
}