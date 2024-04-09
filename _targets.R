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
             "tools", "janitor", "lubridate",
             "PanelMatch"), # Packages that your targets need for their tasks.
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
  tar_target(parlamentary_allowance_data, clean_parlamentary_data(cota_parlamentar_data1)),
  tar_target(legislators_name_data, get_legislators_name(cota_parlamentar_data1)),
  tar_target(legislators_name_data_cleaned, clean_names_legislators(legislators_name_data, "tx_nome_parlamentar")),
  tar_target(tweet_data1_cleaned, clean_names_legislators(tweet_data1, "nome_deputado")),
  tar_target(legislators_1st_match_data, names_first_match(legislators_name_data_cleaned, tweet_data1_cleaned)),
  tar_target(tweet_by_month_df, tweets_by_month(legislators_1st_match_data)),
  tar_target(spending_by_month_df, allowance_by_month(parlamentary_allowance_data)),
  tar_target(df_panel_match, prep_data_panelmatch(spending_by_month_df,tweet_by_month_df )),
  tar_target(plot_desc1, plot_spending_desc(df_panel_match)),
  tar_target(plot_status_treatment, plot_status(df_panel_match)),
  tar_target(pm_results_maha_spending, pm_maha_spending(df_panel_match)),
  tar_target(pm_results_maha_net_spending, pm_maha_net_spending(df_panel_match)),
  tar_target(pm_results_maha_cashback, pm_maha_cash(df_panel_match)),
  tar_target(plot_teste, teste_pm(pm_results_maha_spending)),
  tar_target(reg_twfe_results_spending, reg_twfe(pm_results_maha_spending, df_panel_match)),
  tar_target(reg_twfe_results_net_spending, reg_twfe(pm_results_maha_net_spending, df_panel_match)),
  tar_target(reg_twfe_results_cashback, reg_twfe(pm_results_maha_cashback, df_panel_match)),
  tar_target(leg_tweet_allowance_joined, join_by_cpf(legislators_1st_match_data, parlamentary_allowance_data))
)
