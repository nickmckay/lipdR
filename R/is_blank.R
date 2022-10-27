#helper function to find invalid entries
is_blank <- function(x, false.triggers=FALSE){
  if(is.function(x)) return(FALSE) # Some of the tests below trigger
  # warnings when used on functions
  return(
    is.null(x) ||                # Actually this line is unnecessary since
      length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
      all(is.na(x)) ||
      all(x=="") ||
      all(x=="NA") ||
      (false.triggers && all(!x))
  )
}

is.valid.vec <- function(vec1){
  
  isValid <- rep(NA, length(vec1))
  for(cvc in 1:length(vec1)){
    isValid[cvc] <- !is_blank(vec1[cvc])
  }
  return(isValid)
}