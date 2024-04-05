# R console
library(targets)

# Restart your R session.
rstudioapi::restartSession()

# Loads globals like tar_option_set() packages, simulate_data(), and analyze_data():
tar_load_globals()

# Load the data that the target depends on.
tar_load(cota_parlamentar_data1)

teste <- clean_parlamentary_data(cota_parlamentar_data1)

cota_parlamentar_data1 <- cota_parlamentar_data1 %>%
  clean_names() %>%
  filter(!is.na(dat_emissao)) %>% # !!sym(var_name)
  mutate(dat_emissao1 = as_date(dat_emissao))

sum(is.na(as.factor(cota_parlamentar_data1$dat_emissao1)))
x <- cota_parlamentar_data1[is.na(cota_parlamentar_data1$dat_emissao1),]
head(x)
summary(as.factor(x))
View(tar_meta(fields = error, complete_only = TRUE))
summary(as.factor(x$txt_descricao))

tar_load(tweet_data1_cleaned)
df1 <- add_legislature(tweet_data1)

df <- clean_names_legislators(tweet_data1, "nome_deputado")
glimpse(df)
# Run the command of the errored target.
df <- names_first_match(legislators_name_data_cleaned, tweet_data1_cleaned)

#> Error in na.fail.default(list(measurement = c(1L, 2L, 3L, 4L, 1L, 2L,  : 
#>   missing values in object