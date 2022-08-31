library(tidyverse)
library(zoo)
library(tsibble)
library(lubridate)

convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

num.na <- function(tibble){
  na_matrix <-  tibble[3:length(tibble)] %>% slice() %>% is.na()
 return(rowSums(na_matrix))
}

remove.all.na <- function(tibble){
 na_matrix <-  tibble[3:length(tibble)] %>% slice() %>% is.na()
 na_list <- tibble[rowSums(na_matrix)>=5,] %>% select(Country_Code) %>% pull()
 return (tibble %>% filter(!Country_Code %in% na_list))
}

remove.all.na.totalExp <- function(tibble){
  na_matrix <-  tibble[2:length(tibble)] %>% slice() %>% is.na()
  na_list <- tibble[rowSums(na_matrix)>=5,] %>% select(Country) %>% pull()
  return (tibble %>% filter(!Country %in% na_list))
}

fill.with.average <- function(long_clean_tibble){
  return( na.aggregate(long_clean_tibble %>%
                 select(!Country_Code ) %>% 
                 pivot_wider(values_from = (long_clean_tibble %>%
                                              select(!Country_Code ) %>% names()) [3], 
                             names_from = Country) %>% select(!year)) )
}

fill.with.average.totalExp <- function(long_clean_tibble){
  return( na.aggregate(long_clean_tibble  %>% 
                         pivot_wider(values_from = (long_clean_tibble  %>% names()) [3], 
                                     names_from = Country) %>% select(!year)) )
}

income.classify <- function(input_year, input_gni){
  
  High = criteria %>% filter(year == input_year,
                             category == "High") %>% pull(criteria)
  
  Upper = criteria %>% filter(year == input_year,
                              category == "Upper-middle") %>% pull(criteria)
  
  Lower = criteria %>% filter(year == input_year,
                              category == "Lower-middle") %>% pull(criteria)
  
 if (input_gni >= High){
   return("High")
 } else if (input_gni >= Upper && input_gni < High ) {
   return("Upper")
 } else if(input_gni >= Lower && input_gni < Upper){
   return("Lower")
 } else if(input_gni < Lower){
   return("Low")
 }

#  if_else(input_gni >= High, return("High"), if_else(input_gni >= Upper && input_gni< High, return("Upper"),
#          if_else(input_gni >= Lower && input_gni < Upper, return("Lower"), return("Low"))))
  
  
}