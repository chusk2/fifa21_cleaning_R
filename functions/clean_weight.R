clean_weight <- function(x) {
  if (str_detect(x, 'kg')) {
    weight <- as.numeric(substr(x, 1, nchar(x)-2))
  }
  
  else {
    weight <- substr(x, 1, nchar(x)-3)
    # convert to kg
    weight <- as.numeric(weight) * 0.4535924
  }
  return(weight)
}