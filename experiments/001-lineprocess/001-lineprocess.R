#------------------------------------------------------------------------------#
# Analysis of 001-lineprocess benchmark results
#------------------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

dt <- read_csv(
  file = "experiments/001-lineprocess/benchmarks.csv",
  trim_ws = TRUE,
  skip = 1,
  col_names =
    c("name", "mean_ps", "stdev_ps", "mem_allocated", "mem_copied", "mem_peak")
  )

# Group processing results
group_results <-
  dt[grepl("group experiments", dt$name), ] %>%
  mutate(
    option = stringr::str_extract(name, "option (A|B|C|D)"),
    inputs = 
      stringr::str_extract(name, "(last pass|all pass|first pass|all fail)"),
    nlines = as.integer(stringr::str_extract(name, "[0-1]+$")),
    mean_s = mean_ps * 1e-12
  ) %>%
  dplyr::select(-name)

ggplot(group_results,
   aes(x = log10(nlines), y = log10(mean_s), group = option, color = option)) +
  geom_point() +
  geom_line() +
  facet_grid(inputs ~ .)

app_results <-
  dt[grepl("app experiments", dt$name), ] %>%
  mutate(
    option = stringr::str_extract(name, "option(A|B|C|D|E|F)"),
    inputs =
      stringr::str_extract(name, "(last-pass|all-pass|first-pass|all-fail)"),
    nlines = stringr::str_extract(name, "[0-1]+lines"),
    ngroups =  stringr::str_extract(name, "[0-1]+groups"),
    nlines = as.integer(stringr::str_extract(nlines, "[0-1]+")),
    ngroups = as.integer(stringr::str_extract(ngroups, "[0-1]+")),
    mean_s = mean_ps * 1e-12
  ) %>%
  dplyr::select(-name)

ggplot(app_results,
   aes(x = log10(nlines), y = log10(mean_s), group = option, color = option)) +
  geom_point() +
  geom_line() +
  facet_grid(inputs ~ .)
