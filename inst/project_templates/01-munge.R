# Load Packages ----------------------------------------------------------------
library(ezfun)
library(dplyr)
library(readr)


# Import Data ------------------------------------------------------------------

df0 <-
  read_csv(
    file = here::here("data", ".csv")
  )  |>
  janitor::clean_names() |>
  janitor::remove_empty()


# Clean Data -------------------------------------------------------------------
df <-
  df0 |>
  mutate(

  )


# label data
df <-
  df |>
  labelled::set_variable_labels(

  )


# Save Data --------------------------------------------------------------------
save(
  df,
  file = here::here("data", ".rda"))


# Data Checks  -----------------------------------------------------------------
