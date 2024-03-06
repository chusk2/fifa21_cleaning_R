clean_height <- function(x) {
  if (str_detect(x, 'cm')) {
    height <- as.integer(substr(x, 1, nchar(x)-2))
  }
  
  else {
    heights <- unlist(str_split(x, "'"))
    feet <- heights[1]
    inches <- str_replace(
      unlist(
        str_split("5'10\"", "'") )[2], '"', ''
      )
    
    # convert to cm
    height <- as.integer(feet) * 30.48 + as.integer(inches) * 2.54
  }
  return(height)
}