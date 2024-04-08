## Run targets

library(targets)
library(janitor)
tar_manifest()
tar_visnetwork()

# run make file
tar_make()
# targets::tar_meta(fields = warnings, complete_only = TRUE)

tar_load(cota_parlamentar_data1)
df2 <- clean_parlamentary_data(cota_parlamentar_data1)
glimpse(df2)

tar_load(legislators_1st_match_data)
glimpse(legislators_1st_match_data)
