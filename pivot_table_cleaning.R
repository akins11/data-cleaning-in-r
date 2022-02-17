# libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Data
data_path <- "uncleaned data/4.-Badly-Structured-Sales-Data-4.xlsx"

data <- readxl::read_xlsx(data_path)

# default assigned names
names(data)

# data cleaning
# The data was edited using the Pivot Table tool in Microsoft Excel.
# The data will be converted from a wide format (the current state) to a long
# format.

data <- data %>% 
  # rename the column from multi-index row  to a single row.
  rename(order_id = ...1, order_date = `Ship Mode`, 
         `first class_consumer` = `First Class`, 
         `first class_corporate` = ...4,
         `first class_home office` = ...5,
         `same day_consumer` = `Same Day`,
         `same day_corporate` = ...7,
         `same day_home office` = ...8,
         `second class_consumer` = `Second Class`,
         `second class_corporate` = ...10,
         `second class_home office` = ...11,
         `standard class_consumer` = `Standard Class`,
         `standard class_corporate` = ...13,
         `standard class_home office` = ...14) %>% 
  # dropping the first two rows with the multi-index columns.
  slice(-c(1, 2)) %>% 
  # reshape the data into a long format by introducing two new columns 
  # group_name and sales.
  pivot_longer(`first class_consumer`:`standard class_home office`,
               names_to = "group_name", values_to = "sales") %>% 
  separate(group_name, c("ship_mode", "segment"), sep = "_") %>% 
  filter(order_date != "Grand Total") %>% 
  mutate(order_date = as.numeric(order_date)|>as_date(origin = "1899-12-30"),
         sales = as.numeric(sales)) %>% 
  filter(!is.na(sales)) %>% 
  arrange(order_date)

# Cleaned Data
print(data)

