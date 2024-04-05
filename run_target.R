## Run targets

library(targets)
tar_manifest()
tar_visnetwork()

# run make file
tar_make()
# targets::tar_meta(fields = warnings, complete_only = TRUE)

tar_read(parlamentary_allowance_data)