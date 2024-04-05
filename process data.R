# import data

library(data.table)
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

tweet_2 <- fread(here("Raw data", "Rosie_22October2021_10November 2022.csv"))
tweet_1 <- fread(here("Raw data", "rosiedaserenata_tweets1_8april2017_16Sep2021.csv"))

# parlamentary allowance
cota19 <- fread("Raw data/Ano-2019.csv")
cota19 <- cota19 %>%
  clean_names() %>%
  filter(!is.na(dat_emissao))

cota19 <- cota19 %>%
  mutate(year_month_payment = paste(year(dat_emissao),month(dat_emissao), sep="-"),
         year_month_refund = paste(year(dat_pagamento_restituicao),month(dat_pagamento_restituicao),sep="-")) 

cota19_spending <- cota19 %>%
  group_by(cpf, tx_nome_parlamentar, nu_legislatura, year_month_payment) %>%
  summarise(spending = sum(as.numeric(vlr_liquido)))

cota19_refund <- cota19 %>%
  filter(!is.na(vlr_restituicao)) %>%
  group_by(cpf, tx_nome_parlamentar, nu_legislatura, year_month_refund) %>%
  reframe(spending = as.numeric(vlr_restituicao))

tweet_nomes <- read_excel("Raw data/v2 COM NOMES.rosiedaserenata_tweets1_8April2017 to 16Sep2021.xlsx", 
                                                                           skip = 1, col_types  = c("numeric", "date", "numeric", 
                                                                                                    "numeric", "text", "text", "text"))

glimpse(tweet_nomes)
