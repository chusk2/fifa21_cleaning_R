contract_dates <- function (values) {
  # initialize the vectors
  from <- as.Date(vector())
  to <- as.Date(vector())
  on_loan <- vector('logical')
  
  for (x in values) {
    
    if (str_detect(x, '~')) {
      years <- unlist( str_split(x, ' ~ ') )
      from <- c(from, lubridate::ym( paste0(years[1], '-01') ) )
      to <- c(to, lubridate::ym( paste0(years[2], '-01') ) )
      on_loan <- c(on_loan, FALSE)
    }
    
    else if (x== 'Free') {
      from <- c(from, NA)
      to <- c(to, NA)
      on_loan <- c(on_loan, FALSE)
    }
    
    else {
      info <- unlist(str_split(x, ' On Loan'))
      from <- c(from, lubridate::mdy(info[1]) )
      to <- c(to, NA)
      on_loan <- c(on_loan, TRUE)
    }
  }
  # create new variables in the dataset and assign the results
  dataframe <- data.frame(contract_start = from,
                          contract_end = to, on_loan = on_loan)
  dataframe <- as_tibble(dataframe)
  return(dataframe)
}