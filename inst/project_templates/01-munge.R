# Load Packages ----------------------------------------------------------------
library(ezfun)
library(tidyverse)


# Import Data ------------------------------------------------------------------

raw_file_name <- ""

df0 <-
  read_csv(
    path = here::here("data", raw_file_name)
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()


# Clean Data -------------------------------------------------------------------
df <-
  df0 %>%
  mutate(

  )


# label data
df <-
  df %>%
  labelled::set_variable_labels(

  )


# Save Data --------------------------------------------------------------------
save(
  df,
  path = here::here("data", paste0("raw_file_name", "_data.rda")))


# Data Checks  -----------------------------------------------------------------
