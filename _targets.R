# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
packages = c("tidyverse", "data.table", "readxl", "here",
             "tools", "janitor"), # Packages that your targets need for their tasks.
format = "rds", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own: 1.Cota_parlamentar_2011 to 2022_csv.csv
list(
  tar_target(tweet_file1, "Raw data/v2 COM NOMES.rosiedaserenata_tweets1_8April2017 to 16Sep2021.xlsx", format= "file"),
  tar_target(tweet_file2, "Raw data/Rosie_22October2021_10November 2022.csv", format= "file"),
  tar_target(legislator_file, "Raw data/Cota_parlamentar_2011 to 2022.csv", format= "file"),
  tar_target(tweet_data1, get_raw_data(tweet_file1)),
  tar_target(tweet_data2, get_raw_data(tweet_file2)),
  tar_target(cota_parlamentar_data1, get_raw_data(legislator_file)),
  tar_target(legislators_name_data, get_legislators_name(cota_parlamentar_data1)),
  tar_target(legislators_name_data_cleaned, clean_names_legislators(legislators_name_data, "txNomeParlamentar")),
  tar_target(tweet_data2_cleaned, clean_names_legislators(tweet_data2, "nome_deputado")),
  tar_target(legislators_1st_match_data, names_first_match(legislators_name_data, tweet_data1)),
  tar_target(summary_match, print_summary(legislators_1st_match_data)))