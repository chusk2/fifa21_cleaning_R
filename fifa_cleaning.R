getwd()

# import tidyverse
library(tidyverse)

# import dataset and variable dictionary
dict <- read_csv('variable_descriptions.csv')
df <- read_csv('fifa21_raw_data.csv')

# inspect dataset
str(df)

# variable names
names(df)

# I will get only those variables related to player's contracts
df <- df[, 1:22]
df <- df |> select(-c(photoUrl, playerUrl, Positions, '↓OVA', POT,
                      ID, foot, BOV, BP, Growth))

# Remove Name and rename LongName by Name
df <- df |> select(-Name) |> rename(Name = LongName)

# set lowercase names for variables
df <- df |> rename_with(function(x) str_replace_all(tolower(x), ' ', '_') )

# get a sample of 5 players from the dataset
sample_n(df, 5) |> t()

# Clean text variables

# Clean the club variable
# the club and contract values are together in a same column

# first trim the values to remove new line characters
df$`team_&_contract` <- str_trim(df$`team_&_contract`)
# second is to separate into team and contract
df <- separate(df, 'team_&_contract', into=c('club', 'contract'), sep='\n')

# clean club variable
df$club <- str_trim(df$club)

# check the result by getting the list of unique clubs
df$club |> unique()

# some club names have '1. ' at the beginning. I will remove it.
df$club <- str_replace_all(df$club, '1. ', '')

# clean the value, wage and release clause

# I suspect that all of the values for these three variables have
# the euro sign in front of the value.
all( lapply(df$wage, function(x) substr(x, 1,1) ) == '€' )

# some of the values are in thousands and other are in millions.
# i will create a custom function to set all the values in thousands

clean_qty <- function (x) {
  # remove euro sign
  qty <- substr(x, 2,nchar(x))
  
  # wage of zero
  if (qty == '0') {qty = 0}
  
  # convert from million to thousands
  else if (substr(qty, nchar(qty), nchar(qty)) == 'M') {
    qty <- as.integer( substr(qty, 1, nchar(qty)-1) ) *1E3
  }
  
  # if quantity in thousands, just remove K
  else if (substr(qty, nchar(qty), nchar(qty)) == 'K') {
    qty <- as.integer( substr(qty, 1, nchar(qty)-1) )
  }
  
  # if quantity below thousands, divide by 1000
  else {
    qty <- as.integer( substr(qty, 2, nchar(qty)) ) * 1E-3
  }
  
  return(qty)
}

# clean the data from variables: wage, value and 
economic_vars <- c('value', 'wage', 'release_clause')

for (var in economic_vars) {
  df[[var]] <- sapply(df[[var]], clean_qty)
}

# rename the variables to include units
df <- df |> rename('value(K)' = value, 'wage(K)' = wage,
             'release_clause(K)' = 'release_clause')

# convert to dates the variables: contract, loan_date_end, and joined

df$joined <- lubridate::mdy(df$joined)

# separate the start and end values from contract data

# function contract_dates() defined in external R script
source('contract_dates.R')

# get the start, end and on_loan values using function
# (it takes a couple of minutes to run)
contract_date_values <- contract_dates(df$contract)

# remove contract column and bind the new columns
df <- df |> select(-contract)
df <- as_tibble( cbind(df, contract_date_values ) )

# rearrange the columns
df <- df |> select(name, nationality, age, height, weight, club,
                   joined, contract_start, contract_end, on_loan,
                   loan_date_end, `value(K)`, `wage(K)`, `release_clause(K)`)

# clean the height variable
# load clean_height() function
source('clean_height.R')

# apply to clean height values
df$height <- unlist( lapply(df$height, clean_height) )

# clean the weight variable
# do all the weight values end with kg or lbs?
all(endsWith(df$weight, 'lbs') | endsWith(df$weight, 'kg') )

# load clean_height() function
source('clean_weight.R')

# apply to clean height values
df$weight <- unlist( lapply(df$weight, clean_weight) )

# rename height and weight to include units
df <- df |> rename('height(cm)' = height, 'weight(kg)' = weight)

# set type of age, height and weight to integer
df$age <- as.integer(df$age)
df$'height(cm)' <- as.integer(df$'height(cm)')
df$'weight(kg)' <- as.integer(df$'weight(kg)')

# arrange by name and save as csv file

df <- df |> arrange(name)
write.csv(df, 'fifa_clean_dataset.csv', row.names = FALSE)
