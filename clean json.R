# Libraries ===================================================================|
library(tidyverse) |> suppressPackageStartupMessages()
library(jsonlite) |> suppressPackageStartupMessages()

# Data ========================================================================|
data <- read_csv("uncleaned data\tmdb_5000_movies.csv")

names(data)

# The class of the json columns are all characters.
map_df(data, class)


# Function for creating a data frame from each json string
# NOTE: some of the json strings are missing/empty for some movies.
join_fromJson <- function(x, df_names, new_col_names) {
  chr_vec <- c("name", "iso_3166_1", "iso_639_1")
  
  f_tbl <- fromJSON(x) 
  
  if (class(f_tbl) != "data.frame") {
    f_df <- data.frame(v1 = NA, v2 = NA)
    names(f_df) <- df_names
    
    if (df_names[[1]] %in% chr_vec & df_names[[2]] == "id") {
      f_df[[df_names[[1]]]] <- "none"
      f_df$id <- 0
      
    } else if (df_names[[1]] == "id" & df_names[[2]] %in% chr_vec) {
      f_df[[df_names[[2]]]] <- "none"
      f_df$id <- 0
      
    } else if (df_names[[1]] %in% chr_vec & df_names[[2]] %in% chr_vec) {
      f_df[[df_names[[1]]]] <- "none"
      f_df[[df_names[[2]]]] <- "none"
    }
    f_tbl <- f_df
    
  } else {
    f_tbl 
  }
  
  f_tbl <- f_tbl %>% 
    dplyr::rename("{new_col_names[[1]]}" := 1,
                  "{new_col_names[[2]]}" := 2)
  
  return(f_tbl)
}


# Function for mapping all the data frames created from the json string into a
# list column using the custom join_fromJson() function.
list_col <- function(var, col_name, new_col_name) {
  
 purrr::map({{ var }}, join_fromJson, col_name, new_col_name)
}


# Function to getting the column names of the data frame converted from json 
# sting using the jsonlite::fromJSON() function.
get_col_names <- function(df, var) {
  df[[var]][[1]] |> jsonlite::fromJSON() |> names()
}


# data cleaning 
data <- data %>% 
  mutate(
    genres = list_col(
      var = genres, 
      col_name = get_col_names(data, "genres"), 
      new_col_name =  c("genre_id", "genre")
    ),
    keywords = list_col(
      var = keywords, 
      col_name = get_col_names(data, "keywords"), 
      new_col_name = c("keyword_id", "keyword")
    ),
    production_companies = list_col(
      var = production_companies,
      col_name = get_col_names(data, "production_companies"),
      new_col_name = c("production_company", "prod_company_id")
    ),
    production_countries = list_col(
      var = production_countries,
      col_name = get_col_names(data, "production_countries"),
      new_col_name = c("iso_3166_1", "production_country")
    ),
    spoken_languages = list_col(
      var = spoken_languages, 
      col_name = get_col_names(data, "spoken_languages"),
      new_col_name = c("iso_639_1", "language")
    )
  )


# all nested columns
data %>% 
  select(genres, keywords, production_companies, production_countries, 
         spoken_languages)


# unnesting columns
data %>% 
  select(id, title, genres) %>% 
  unnest(cols = genres)

data %>%
  select(id, title, production_companies, production_countries) %>%
  unnest(cols = production_companies) %>%
  unnest(cols = production_countries)

write_csv(data, "movies.csv")