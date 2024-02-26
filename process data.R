# import data

library(data.table)
library(here)
library(tidyverse)
library(readxl)

tweet_2 <- fread(here("Raw data", "Rosie_22October2021_10November 2022.csv"))
tweet_1 <- fread(here("Raw data", "rosiedaserenata_tweets1_8april2017_16Sep2021.csv"))



tweet_nomes <- read_excel("Raw data/v2 COM NOMES.rosiedaserenata_tweets1_8April2017 to 16Sep2021.xlsx", 
                                                                           skip = 1, col_types  = c("numeric", "date", "numeric", 
                                                                                                    "numeric", "text", "text", "text"))

glimpse(tweet_nomes)
