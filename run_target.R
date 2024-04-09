## Run targets

library(targets)
library(janitor)
tar_manifest()
tar_visnetwork()

# run make file
tar_make()
# targets::tar_meta(fields = warnings, complete_only = TRUE)

tar_load(reg_twfe_results_spending)
tar_load(reg_twfe_results_net_spending)
tar_load(reg_twfe_results_cashback)

plot(reg_twfe_results_spending)
plot(reg_twfe_results_net_spending)
plot(reg_twfe_results_cashback)
