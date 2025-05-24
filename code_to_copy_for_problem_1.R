library(tidyverse)
library(readxl)
setwd(Put your path file here)
incarceration_data <- read_xlsx("incarceration_counts_and_rates_by_type_over_time.xlsx",
                                range = "A7:CO10") %>%
  rename("type" = ...1) %>%
  pivot_longer(`1925`:`2016`, names_to = "year", values_to = "counts")