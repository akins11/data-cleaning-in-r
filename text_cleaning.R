# Useful libraries ------------------------------------------------------------
library(stringr)
library(dplyr)

# Data ------------------------------------------------------------------------
data_path <- "uncleaned data/5.-Jumbled-up-Customers-Details.xlsx"
data <- readxl::read_xlsx(data_path, col_names = FALSE)


# Data inspection
print(data)  

# The data contains only one column (...1) which was created by the function
# read_xlsx from the readxl package because there are no column names for 
# the data. The column is a combination of four different variable namely 
# 1. The name of the individual.
# 2. The individual address.
# 3. The individual age.
# 4. The gender of the individual.

# Given that all the variables are within a single text for each row, using
# regular expressions will be one of the best options.

# First of we will use the first row observation in the data to test the best
# reg-ex to use

data_string <- data$...1[1]

# Each variable to extract begins with it title such as Name ... Address ... and 
# So on, The str_extract() function from the stringr package will be used for 
# the extraction.

# everything after the title Name
str_extract(data_string, "(?<=Name).*")

# everything before the title Address 
word(data_string, 1, sep = "Address")


# Extracting the Name 
str_extract(data_string, "(?<=Name).*(?=Address)") |> 
  # remove the white space on both side of the name using str_trim function
  str_trim()

# Extracting the Address
str_extract(data_string, "(?<=Address).*(?=Age)") |> 
  str_trim()

# Extracting the Age
str_extract(data_string, "(?<=Age).*(?=Gender)") |> 
  str_trim()

# Extracting the Gender
str_extract(data_string, "(?<=Gender).*") |> 
  str_trim()


# so we can now create a function to avoid repetition of the reg-ex
str_range_ex <- function(string, between) {
  stopifnot(inherits(string, "character"))
  
  if (length(between) < 2 | length(between) > 2) {
    stop("argument `between` must be a vector of 2 characters")
  }
  
  
  str_pattern <- paste0("(?<=", between[1] ,").*(?=", between[2] ,")")
  
  output <- stringr::str_extract(string, pattern = str_pattern) |>
    stringr::str_trim()
  
  return(output)
}

# Extracting the name using the str_range_ex function
str_range_ex(data_string, c("Name", "Address"))

# Note that the the match is case sensitive, when name is used instead of Name
# the result will be NA.


# Cleaning the data
data <- data %>% 
  mutate(name = str_range_ex(...1, between = c("Name", "Address")),
         address = str_range_ex(...1, between = c("Address", "Age")),
         age = str_range_ex(...1, between = c("Age", "Gender")),
         gender = str_range_ex(...1, between = c("Gender", ""))) %>% 
  # Drop the ...1 column
  select(-...1) %>% 
  # change age class from character to numeric
  mutate(age = as.numeric(age))

print(data)
