

library(tidyverse)
library(readr)

dat <- read_csv(url("https://raw.githubusercontent.com/cbrown5/example-ecological-data/refs/heads/main/data/benthic-reefs-and-fish/fish-coral-cover-sites.csv"))

head(dat)
summary(dat)
